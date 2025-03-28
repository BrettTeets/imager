//
// Raster->Chain Tree (Suzuki algorithms)
//

// Structure that is used for sequential retrieving contours from the image.
// It supports both hierarchical and plane variants of Suzuki algorithm.
struct ContourScanner_
{
    ContourPointsStorage::storage_t& pointsStorage;
    ContourCodesStorage::storage_t& codesStorage;

    Mat image;
    Point offset;  // ROI offset: coordinates, added to each contour point
    Point pt;  // current scanner position
    Point lnbd;  // position of the last met contour
    schar nbd;  // current mark val
    int approx_method1;  // approx method when tracing
    int approx_method2;  // final approx method
    int mode;
    CTree tree;
    array<int, 128> ctable;

public:
    ContourScanner_(ContourPointsStorage::storage_t& _pointsStorage,
                    ContourCodesStorage::storage_t& _codesStorage)
                   :pointsStorage(_pointsStorage),codesStorage(_codesStorage) {}
    ~ContourScanner_() {}
    inline bool isInt() const
    {
        return (this->mode == RETR_FLOODFILL);
    }
    inline bool isSimple() const
    {
        return (this->mode == RETR_EXTERNAL || this->mode == RETR_LIST);
    }

    CNode& makeContour(schar& nbd_, const bool is_hole, const int x, const int y);
    bool contourScan(const int prev, int& p, Point& last_pos, const int x, const int y);
    int findFirstBoundingContour(const Point& last_pos, const int y, const int lval, int par);
    int findNextX(int x, int y, int& prev, int& p);
    bool findNext();

    static shared_ptr<ContourScanner_> create(ContourPointsStorage::storage_t& pointsStorage, ContourCodesStorage::storage_t& codesStorage, Mat img, int mode, int method, Point offset);
};  // class ContourScanner_

typedef shared_ptr<ContourScanner_> ContourScanner;


shared_ptr<ContourScanner_> ContourScanner_::create(ContourPointsStorage::storage_t& pointsStorage, ContourCodesStorage::storage_t& codesStorage, Mat img, int mode, int method, Point offset)
{
    if (mode == RETR_CCOMP && img.type() == CV_32SC1)
        mode = RETR_FLOODFILL;

    if (mode == RETR_FLOODFILL)
        CV_CheckTypeEQ(img.type(), CV_32SC1, "RETR_FLOODFILL mode supports only CV_32SC1 images");
    else
        CV_CheckTypeEQ(img.type(),
                       CV_8UC1,
                       "Modes other than RETR_FLOODFILL and RETR_CCOMP support only CV_8UC1 "
                       "images");

    CV_Check(mode,
             mode == RETR_EXTERNAL || mode == RETR_LIST || mode == RETR_CCOMP ||
                 mode == RETR_TREE || mode == RETR_FLOODFILL,
             "Wrong extraction mode");

    CV_Check(method,
             method == 0 || method == CHAIN_APPROX_NONE || method == CHAIN_APPROX_SIMPLE ||
                 method == CHAIN_APPROX_TC89_L1 || method == CHAIN_APPROX_TC89_KCOS,
             "Wrong approximation method");

    Size size = img.size();
    CV_Assert(size.height >= 1);

    shared_ptr<ContourScanner_> scanner = make_shared<ContourScanner_>(pointsStorage, codesStorage);
    scanner->image = img;
    scanner->mode = mode;
    scanner->offset = offset;
    scanner->pt = Point(1, 1);
    scanner->lnbd = Point(0, 1);
    scanner->nbd = 2;
    CNode& root = scanner->tree.newElem(Contour(&scanner->pointsStorage, &scanner->codesStorage));
    CV_Assert(root.self() == 0);
    root.body.isHole = true;
    root.body.brect = Rect(Point(0, 0), size);
    scanner->ctable.fill(-1);
    scanner->approx_method2 = scanner->approx_method1 = method;
    if (method == CHAIN_APPROX_TC89_L1 || method == CHAIN_APPROX_TC89_KCOS)
        scanner->approx_method1 = CV_CHAIN_CODE;
    return scanner;
}

CNode& ContourScanner_::makeContour(schar& nbd_, const bool is_hole, const int x, const int y)
{
    const bool isChain = (this->approx_method1 == CV_CHAIN_CODE);  // TODO: get rid of old constant
    const bool isDirect = (this->approx_method1 == CHAIN_APPROX_NONE);

    const Point start_pt(x - (is_hole ? 1 : 0), y);

    CNode& res = tree.newElem(Contour(&pointsStorage, &codesStorage));
    res.body.isHole = is_hole;
    res.body.isChain = isChain;
    res.body.origin = start_pt + offset;
    if (isSimple())
    {
        icvFetchContourEx<schar>(this->image, start_pt, MASK8_NEW, res.body, isDirect);
    }
    else
    {
        schar lval;
        if (isInt())
        {
            const int start_val = this->image.at<int>(start_pt);
            lval = start_val & MASK8_LVAL;
            icvFetchContourEx<int>(this->image, start_pt, 0, res.body, isDirect);
        }
        else
        {
            lval = nbd_;
            // change nbd
            nbd_ = (nbd_ + 1) & MASK8_LVAL;
            if (nbd_ == 0)
                nbd_ = MASK8_BLACK | MASK8_NEW;
            icvFetchContourEx<schar>(this->image, start_pt, lval, res.body, isDirect);
        }
        res.body.brect.x -= this->offset.x;
        res.body.brect.y -= this->offset.y;
        res.ctable_next = this->ctable[lval];
        this->ctable[lval] = res.self();
    }
    const Point prev_origin = res.body.origin;
    res.body.origin = start_pt;
    if (this->approx_method1 != this->approx_method2)
    {
        CV_Assert(res.body.isChain);
        approximateChainTC89(res.body.codes, prev_origin, this->approx_method2, res.body.pts);
        res.body.isChain = false;
    }
    return res;
}

bool ContourScanner_::contourScan(const int prev, int& p, Point& last_pos, const int x, const int y)
{
    bool is_hole = false;

    /* if not external contour */
    if (isInt())
    {
        if (!(((prev & MASK_FLAGS) != 0 || prev == 0) && (p & MASK_FLAGS) == 0))
        {
            if ((prev & MASK_FLAGS) != 0 || ((p & MASK_FLAGS) != 0))
                return false;

            if (prev & MASK_FLAGS)
            {
                last_pos.x = x - 1;
            }
            is_hole = true;
        }
    }
    else
    {
        //BRETT: This is one of the first steps in the algo.
        /*Scan an input binary picture with a TV raster and interrupt the
            raster scan when a pixel (i, j) is found which satisfies the condition for the border
            following starting point of either an outer border (Fig. 2a) or a hole border (Fig. 2b).
            If the pixel (i, j) satisfies both of the above conditions, (i, j) must be regarded as
            the starting point of the outer border.*/
        //             j-1    j                     j     j+1
        //fig:2a  i |  0  |  1  |     fig:2b  i | >= 1 |  0   |
        if (!(prev == 0 && p == 1)){  
            
            if (p != 0 || prev < 1) return false;

            if (prev & MASK8_FLAGS) { last_pos.x = x - 1; }
            is_hole = true;
        }
    }

    if (mode == RETR_EXTERNAL && (is_hole || this->image.at<schar>(last_pos) > 0))
    {
        return false;
    }

    /* find contour parent */
    int main_parent = -1;
    if (isSimple() || (!is_hole && (mode == RETR_CCOMP || mode == RETR_FLOODFILL)) ||
        last_pos.x <= 0)
    {
        main_parent = 0;
    }
    else
    {
        int lval;
        if (isInt())
            lval = this->image.at<int>(last_pos.y, last_pos.x) & MASK8_LVAL;
        else
            lval = this->image.at<schar>(last_pos.y, last_pos.x) & MASK8_LVAL;

        main_parent = findFirstBoundingContour(last_pos, y, lval, main_parent);

        // if current contour is a hole and previous contour is a hole or
        // current contour is external and previous contour is external then
        // the parent of the contour is the parent of the previous contour else
        // the parent is the previous contour itself.
        {
            CNode& main_parent_elem = tree.elem(main_parent);
            if (main_parent_elem.body.isHole == is_hole)
            {
                if (main_parent_elem.parent != -1)
                {
                    main_parent = main_parent_elem.parent;
                }
                else
                {
                    main_parent = 0;
                }
            }
        }

        // hole flag of the parent must differ from the flag of the contour
        {
            CNode& main_parent_elem = tree.elem(main_parent);
            CV_Assert(main_parent_elem.body.isHole != is_hole);
        }
    }

    last_pos.x = x - (is_hole ? 1 : 0);

    schar nbd_ = this->nbd;
    CNode& new_contour = makeContour(nbd_, is_hole, x, y);
    if (new_contour.parent == -1)
    {
        tree.addChild(main_parent, new_contour.self());
    }
    this->pt.x = !isInt() ? (x + 1) : (x + 1 - (is_hole ? 1 : 0));
    this->pt.y = y;
    this->nbd = nbd_;
    return true;
}

int ContourScanner_::findFirstBoundingContour(const Point& last_pos,
                                              const int y,
                                              const int lval,
                                              int par)
{
    const Point end_point(last_pos.x, y);
    int res = par;
    int cur = ctable[lval];
    while (cur != -1)
    {
        CNode& cur_elem = tree.elem(cur);
        if (((last_pos.x - cur_elem.body.brect.x) < cur_elem.body.brect.width) &&
            ((last_pos.y - cur_elem.body.brect.y) < cur_elem.body.brect.height))
        {
            if (res != -1)
            {
                CNode& res_elem = tree.elem(res);
                const Point origin = res_elem.body.origin;
                const bool isHole = res_elem.body.isHole;
                if (isInt())
                {
                    if (icvTraceContour<int>(this->image, origin, end_point, isHole))
                        break;
                }
                else
                {
                    if (icvTraceContour<schar>(this->image, origin, end_point, isHole))
                        break;
                }
            }
            res = cur;
        }
        cur = cur_elem.ctable_next;
    }
    return res;
}

int ContourScanner_::findNextX(int x, int y, int& prev, int& p)
{
    const int width = this->image.size().width - 1;
    if (isInt())
    {
        for (; x < width &&
               ((p = this->image.at<int>(y, x)) == prev || (p & MASK_VAL) == (prev & MASK_VAL));
             x++)
            prev = p;
    }
    else
    {
#if (CV_SIMD || CV_SIMD_SCALABLE)
        if ((p = this->image.at<schar>(y, x)) != prev)
        {
            return x;
        }
        else
        {
            v_uint8 v_prev = vx_setall_u8((uchar)prev);
            for (; x <= width - VTraits<v_uint8>::vlanes(); x += VTraits<v_uint8>::vlanes())
            {
                v_uint8 vmask = (v_ne(vx_load(this->image.ptr<uchar>(y, x)), v_prev));
                if (v_check_any(vmask))
                {
                    x += v_scan_forward(vmask);
                    p = this->image.at<schar>(y, x);
                    return x;
                }
            }
        }
#endif
        for (; x < width && (p = this->image.at<schar>(y, x)) == prev; x++)
            ;
    }
    return x;
}

bool ContourScanner_::findNext()
{
    int x = this->pt.x;
    int y = this->pt.y;
    int width = this->image.size().width - 1;
    int height = this->image.size().height - 1;
    Point last_pos = this->lnbd;
    int prev = isInt() ? this->image.at<int>(y, x - 1) : this->image.at<schar>(y, x - 1);

    for (; y < height; y++)
    {
        int p = 0;
        for (; x < width; x++)
        {
            x = findNextX(x, y, prev, p);
            if (x >= width)
                break;
            if (contourScan(prev, p, last_pos, x, y))
            {
                this->lnbd = last_pos;
                return true;
            }
            else
            {
                prev = p;
                if ((isInt() && (prev & MASK_FLAGS)) || (!isInt() && (prev & MASK8_FLAGS)))
                {
                    last_pos.x = x;
                }
            }
        }
        last_pos = Point(0, y + 1);
        x = 1;
        prev = 0;
    }

    return false;
}

//==============================================================================