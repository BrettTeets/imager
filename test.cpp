https://github.com/opencv/opencv/blob/01ef38dcad65a119204e6d70f553767a7d2ab614/modules/calib3d/src/calibinit.cpp#L648

bool findChessboardCorners(InputArray image_, Size pattern_size, OutputArray corners_, int flags)
{
    CV_INSTRUMENT_REGION();

    DPRINTF("==== findChessboardCorners(img=%dx%d, pattern=%dx%d, flags=%d)", image_.cols(), image_.rows(), pattern_size.width, pattern_size.height, flags);

    bool found = false;

    const bool is_plain = (flags & CALIB_CB_PLAIN) != 0;

    int type = image_.type(), depth = CV_MAT_DEPTH(type), cn = CV_MAT_CN(type);
    Mat img = image_.getMat();

    std::vector<cv::Point2f> out_corners;

    int prev_sqr_size = 0;

    Mat thresh_img_new = img.clone();
    if(!is_plain) icvBinarizationHistogramBased(thresh_img_new); // process image in-place
    SHOW("New binarization", thresh_img_new);

    if (flags & CALIB_CB_FAST_CHECK && !is_plain)
    {
        //perform new method for checking chessboard using a binary image.
        //image is binarised using a threshold dependent on the image histogram
        //BRETT: So reading the comments this is doing a very quick check to see if a chessboard is present at all
        //otherwise searching on an image without a chessboard takes too long.
        if (checkChessboardBinary(thresh_img_new, pattern_size) <= 0) //fall back to the old method
        { if (!checkChessboard(img, pattern_size)) { corners_.release(); return false;}}
    }

    ChessBoardDetector detector(pattern_size);

    const int min_dilations = 0;
    const int max_dilations = is_plain ? 0 : 7;

    // Try our standard "0" and "1" dilations, but if the pattern is not found, iterate the whole procedure with higher dilations.
    // This is necessary because some squares simply do not separate properly without and with a single dilations. However,
    // we want to use the minimum number of dilations possible since dilations cause the squares to become smaller,
    // making it difficult to detect smaller squares.
    for (int dilations = min_dilations; dilations <= max_dilations; dilations++)
    {
        //USE BINARY IMAGE COMPUTED USING icvBinarizationHistogramBased METHOD
        if(!is_plain && dilations > 0)
        dilate( thresh_img_new, thresh_img_new, Mat(), Point(-1, -1), 1 );

        // So we can find rectangles that go to the edge, we draw a white line around the image edge.
        // Otherwise FindContours will miss those clipped rectangle contours.
        // The border color will be the image mean, because otherwise we risk screwing up filters like cvSmooth()...
        rectangle( thresh_img_new, Point(0,0), Point(thresh_img_new.cols-1, thresh_img_new.rows-1), Scalar(255,255,255), 3, LINE_8);

        detector.reset();
        detector.generateQuads(thresh_img_new, flags, dilations);
        DPRINTF("Quad count: %d/%d", detector.all_quads_count, (pattern_size.width/2+1)*(pattern_size.height/2+1));
        SHOW_QUADS("New quads", thresh_img_new, &detector.all_quads[0], detector.all_quads_count);
        if (detector.processQuads(out_corners, prev_sqr_size))
        {
            found = true;
            break;
        }
    }

    DPRINTF("Chessboard detection result 0: %d", (int)found);

    // revert to old, slower, method if detection failed
    if (!found && !is_plain)
    {
        if (flags & CALIB_CB_NORMALIZE_IMAGE)
        {
        img = img.clone();
        equalizeHist(img, img);
        }

        Mat thresh_img;
        prev_sqr_size = 0;

        DPRINTF("Fallback to old algorithm");
        const bool useAdaptive = flags & CALIB_CB_ADAPTIVE_THRESH;
        if (!useAdaptive)
        {
        // empiric threshold level
        // thresholding performed here and not inside the cycle to save processing time
        double mean = cv::mean(img).val[0];
        int thresh_level = std::max(cvRound(mean - 10), 10);
        threshold(img, thresh_img, thresh_level, 255, THRESH_BINARY);
        }
        //if flag CALIB_CB_ADAPTIVE_THRESH is not set it doesn't make sense to iterate over k
        int max_k = useAdaptive ? 6 : 1;
        Mat prev_thresh_img;
        for (int k = 0; k < max_k && !found; k++)
        {
            int prev_block_size = -1;
            for (int dilations = min_dilations; dilations <= max_dilations; dilations++)
            {
                // convert the input grayscale image to binary (black-n-white)
                if (useAdaptive){
                    int block_size = cvRound(prev_sqr_size == 0 ? std::min(img.cols, img.rows) * (k % 2 == 0 ? 0.2 : 0.1) : prev_sqr_size * 2);
                    block_size = block_size | 1;
                    // convert to binary
                    if (block_size != prev_block_size) {
                        adaptiveThreshold( img, thresh_img, 255, ADAPTIVE_THRESH_MEAN_C, THRESH_BINARY, block_size, (k/2)*5 );
                        dilate( thresh_img, thresh_img, Mat(), Point(-1, -1), dilations );
                        thresh_img.copyTo(prev_thresh_img);
                    } else if (dilations > 0) {
                        dilate( prev_thresh_img, prev_thresh_img, Mat(), Point(-1, -1), 1 );
                        prev_thresh_img.copyTo(thresh_img);
                    }
                    prev_block_size = block_size;
                } else {
                    if (dilations > 0)
                    dilate( thresh_img, thresh_img, Mat(), Point(-1, -1), 1 );
                }
                SHOW("Old binarization", thresh_img);

                // So we can find rectangles that go to the edge, we draw a white line around the image edge.
                // Otherwise FindContours will miss those clipped rectangle contours.
                // The border color will be the image mean, because otherwise we risk screwing up filters like cvSmooth()...
                rectangle( thresh_img, Point(0,0), Point(thresh_img.cols-1, thresh_img.rows-1), Scalar(255,255,255), 3, LINE_8);

                detector.reset();
                detector.generateQuads(thresh_img, flags, dilations);
                DPRINTF("Quad count: %d/%d", detector.all_quads_count, (pattern_size.width/2+1)*(pattern_size.height/2+1));
                SHOW_QUADS("Old quads", thresh_img, &detector.all_quads[0], detector.all_quads_count);
                if (detector.processQuads(out_corners, prev_sqr_size))
                {
                found = 1;
                break;
                }
            }
        }
    }

    DPRINTF("Chessboard detection result 1: %d", (int)found);

    if (found) found = detector.checkBoardMonotony(out_corners);

    DPRINTF("Chessboard detection result 2: %d", (int)found);

    // check that none of the found corners is too close to the image boundary
    if (found)
    {
        const int BORDER = 8;
        for (int k = 0; k < pattern_size.width*pattern_size.height; ++k)
        {
            if( out_corners[k].x <= BORDER || out_corners[k].x > img.cols - BORDER || out_corners[k].y <= BORDER || out_corners[k].y > img.rows - BORDER )
            {
                found = false;
                break;
            }
        }
    }

    DPRINTF("Chessboard detection result 3: %d", (int)found);

    if (found)
    {
        if ((pattern_size.height & 1) == 0 && (pattern_size.width & 1) == 0 )
        {
            int last_row = (pattern_size.height-1)*pattern_size.width;
            double dy0 = out_corners[last_row].y - out_corners[0].y;
            if (dy0 < 0)
            {
                int n = pattern_size.width*pattern_size.height;
                for(int i = 0; i < n/2; i++ )
                {
                    std::swap(out_corners[i], out_corners[n-i-1]);
                }
            }
        }
        cv::cornerSubPix(img, out_corners, Size(2, 2), Size(-1,-1),
        cv::TermCriteria(TermCriteria::EPS + TermCriteria::MAX_ITER, 15, 0.1));
    }

    Mat(out_corners).copyTo(corners_);
    return found;
}

//The Error handling code for this. OR at the very least the guard statements they use.
//I can probably treat these as invarients.
CV_CheckType(type, depth == CV_8U && (cn == 1 || cn == 3 || cn == 4), "Only 8-bit grayscale or color images are supported");
if (pattern_size.width <= 2 || pattern_size.height <= 2) CV_Error(Error::StsOutOfRange, "Both width and height of the pattern should have bigger than 2");
if (!corners_.needed()) CV_Error(Error::StsNullPtr, "Null pointer to corners");
if (is_plain) CV_CheckType(type, depth == CV_8U && cn == 1, "Only 8-bit grayscale images are supported whith CALIB_CB_PLAIN flag enable");
if (img.channels() != 1) { cvtColor(img, img, COLOR_BGR2GRAY); }


//This is called above.
bool cv::checkChessboard(InputArray _img, Size size)
{
    Mat img = _img.getMat();
    CV_Assert(img.channels() == 1 && img.depth() == CV_8U);

    const int erosion_count = 1;
    const float black_level = 20.f;
    const float white_level = 130.f;
    const float black_white_gap = 70.f;

    //erode image into white,
    //dilate image into black,
    Mat white;
    Mat black;
    erode(img, white, Mat(), Point(-1, -1), erosion_count);
    dilate(img, black, Mat(), Point(-1, -1), erosion_count);

    bool result = false;
    for(float thresh_level = black_level; thresh_level < white_level && !result; thresh_level += 20.0f)
    {
        //thesh_level is increasing by 20.0f every attempt.
        //mat mat, white threshlevel, black threshlevel, output. 
        vector<pair<float, int> > quads;
        fillQuads(white, black, thresh_level + black_white_gap, thresh_level, quads);
        if (checkQuads(quads, size))
            result = true;
    }
    return result;
}

https://docs.opencv.org/3.4/db/df6/tutorial_erosion_dilatation.html
https://github.com/opencv/opencv/blob/4.x/modules/imgproc/src/morph.dispatch.cpp#L1016

void erode( InputArray src, OutputArray dst, InputArray kernel, Point anchor, int iterations, int borderType, const Scalar& borderValue )
{
    CV_INSTRUMENT_REGION();

    CV_Assert(!src.empty());

    morphOp( MORPH_ERODE, src, dst, kernel, anchor, iterations, borderType, borderValue );
}


void dilate( InputArray src, OutputArray dst, InputArray kernel, Point anchor, int iterations, int borderType, const Scalar& borderValue )
{
    CV_INSTRUMENT_REGION();

    CV_Assert(!src.empty());

    morphOp( MORPH_DILATE, src, dst, kernel, anchor, iterations, borderType, borderValue );
}

static void morphOp( int op, InputArray _src, OutputArray _dst,
                     InputArray _kernel,
                     Point anchor, int iterations,
                     int borderType, const Scalar& borderValue )
{
    CV_INSTRUMENT_REGION();

    Mat kernel = _kernel.getMat();
    Size ksize = !kernel.empty() ? kernel.size() : Size(3,3);
    anchor = normalizeAnchor(anchor, ksize);

    CV_OCL_RUN(_dst.isUMat() && _src.dims() <= 2 && _src.channels() <= 4 &&
               borderType == cv::BORDER_CONSTANT && borderValue == morphologyDefaultBorderValue() &&
               (op == MORPH_ERODE || op == MORPH_DILATE) &&
               anchor.x == ksize.width >> 1 && anchor.y == ksize.height >> 1,
               ocl_morphOp(_src, _dst, kernel, anchor, iterations, op, borderType, borderValue) )

    if (iterations == 0 || kernel.rows*kernel.cols == 1)
    {
        _src.copyTo(_dst);
        return;
    }

    if (kernel.empty())
    {
        kernel = getStructuringElement(MORPH_RECT, Size(1+iterations*2,1+iterations*2));
        anchor = Point(iterations, iterations);
        iterations = 1;
    }
    else if( iterations > 1 && countNonZero(kernel) == kernel.rows*kernel.cols )
    {
        anchor = Point(anchor.x*iterations, anchor.y*iterations);
        kernel = getStructuringElement(MORPH_RECT,
                                       Size(ksize.width + (iterations-1)*(ksize.width-1),
                                            ksize.height + (iterations-1)*(ksize.height-1)),
                                       anchor);
        iterations = 1;
    }

    Mat src = _src.getMat();
    _dst.create( src.size(), src.type() );
    Mat dst = _dst.getMat();

    Point s_ofs;
    Size s_wsz(src.cols, src.rows);
    Point d_ofs;
    Size d_wsz(dst.cols, dst.rows);
    bool isolated = (borderType&BORDER_ISOLATED)?true:false;
    borderType = (borderType&~BORDER_ISOLATED);

    if(!isolated)
    {
        src.locateROI(s_wsz, s_ofs);
        dst.locateROI(d_wsz, d_ofs);
    }

    hal::morph(op, src.type(), dst.type(),
               src.data, src.step,
               dst.data, dst.step,
               src.cols, src.rows,
               s_wsz.width, s_wsz.height, s_ofs.x, s_ofs.y,
               d_wsz.width, d_wsz.height, d_ofs.x, d_ofs.y,
               kernel.type(), kernel.data, kernel.step, kernel.cols, kernel.rows, anchor.x, anchor.y,
               borderType, borderValue.val, iterations,
               (src.isSubmatrix() && !isolated));
}

tatic void fillQuads(Mat & white, Mat & black, double white_thresh, double black_thresh, vector<pair<float, int> > & quads)
{
    Mat thresh;
    //One for the white mat
    {
        vector< vector<Point> > contours; //created this
        vector< Vec4i > hierarchy; //create this.
        //mat, output, theshould value, max_Value, flag
        threshold(white, thresh, white_thresh, 255, THRESH_BINARY);
        findContours(thresh, contours, hierarchy, RETR_CCOMP, CHAIN_APPROX_SIMPLE);
        icvGetQuadrangleHypotheses(contours, hierarchy, quads, 1);
    }

    //one for the black mat.
    {
        vector< vector<Point> > contours;
        vector< Vec4i > hierarchy;
        threshold(black, thresh, black_thresh, 255, THRESH_BINARY_INV);
        findContours(thresh, contours, hierarchy, RETR_CCOMP, CHAIN_APPROX_SIMPLE);
        icvGetQuadrangleHypotheses(contours, hierarchy, quads, 0);
    }
}

static bool checkQuads(vector<pair<float, int> > & quads, const cv::Size & size)
{
    const size_t min_quads_count = size.width*size.height/2;
    std::sort(quads.begin(), quads.end(), less_pred);

    // now check if there are many hypotheses with similar sizes
    // do this by floodfill-style algorithm
    const float size_rel_dev = 0.4f;

    for(size_t i = 0; i < quads.size(); i++)
    {
        size_t j = i + 1;
        for(; j < quads.size(); j++)
        {
            if(quads[j].first/quads[i].first > 1.0f + size_rel_dev)
            {
                break;
            }
        }

        if(j + 1 > min_quads_count + i)
        {
            // check the number of black and white squares
            std::vector<int> counts;
            countClasses(quads, i, j, counts);
            const int black_count = cvRound(ceil(size.width/2.0)*ceil(size.height/2.0));
            const int white_count = cvRound(floor(size.width/2.0)*floor(size.height/2.0));
            if(counts[0] < black_count*0.75 ||
               counts[1] < white_count*0.75)
            {
                continue;
            }
            return true;
        }
    }
    return false;
}