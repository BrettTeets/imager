module Chessboard = struct

  

  
end

(* checkboard: https://github.com/opencv/opencv/blob/01ef38dcad65a119204e6d70f553767a7d2ab614/modules/calib3d/src/calibinit.cpp#L648 *)
  (*Binarization historgram based. https://github.com/opencv/opencv/blob/01ef38dcad65a119204e6d70f553767a7d2ab614/modules/calib3d/src/calibinit.cpp#L352 *)
  
  (*ChessBoardDetector detector(pattern_size); making this object here.*)
  
  (* const int min_dilations = 0;
    const int max_dilations = 7;*)

    (* for (int dilations = min_dilations; dilations <= max_dilations; dilations++)
    {
        //USE BINARY IMAGE COMPUTED USING icvBinarizationHistogramBased METHOD
        if(dilations > 0) dilate( thresh_img_new, thresh_img_new, Mat(), Point(-1, -1), 1 );

        (Typically openCV will draw a square around the frame of the image to help findcontours with squares near the edge but I can skip this I think.)

        detector.generateQuads(thresh_img_new, flags, dilations);


        DPRINTF("Quad count: %d/%d", detector.all_quads_count, (pattern_size.width/2+1)*(pattern_size.height/2+1));
        SHOW_QUADS("New quads", thresh_img_new, &detector.all_quads[0], detector.all_quads_count);
        if (detector.processQuads(out_corners, prev_sqr_size))
        {
            found = true;
            break;
        }
    } *)

    (*Then we go into the slower method.*)

(*Chessboard detector 
 pattern size (int * int)
 chessboard quads buffer
 chessboard corners.
 number of quads so far.
 *)

(*Chessboard quad data, num of neirhbors, id, row and colum, ordered counter clockwise, edge squared lenth? point of the corners, pointers to 4- neighbors.*)
(*chessboard corner data, row count, pointer to neighbors.*)

(* void ChessBoardDetector::generateQuads(const cv::Mat& image_, int flags, int dilations)
{
    binarized_image = image_;  // save for debug purposes

    int quad_count = 0;

    all_quads.deallocate();
    all_corners.deallocate();

    // empiric bound for minimal allowed area for squares
    const int min_area = 25; //cvRound( image->cols * image->rows * .03 * 0.01 * 0.92 );

    bool filterQuads = (flags & CALIB_CB_FILTER_QUADS) != 0; (*This gets rid of false quads.*)

    std::vector<std::vector<Point> > contours;
    std::vector<Vec4i> hierarchy;

    cv::findContours(image_, contours, hierarchy, RETR_CCOMP, CHAIN_APPROX_SIMPLE); (*this does the simple chain approximation and organizes the heiarchy into a two teir heiarchy.*)


    std::vector<int> contour_child_counter(contours.size(), 0); (*gets the number of countours and sets them to 0?*)
    int boardIdx = -1;

    std::vector<QuadCountour> contour_quads;  (*quad contours is the four points and the index to the parent contour.*)

    for (int idx = (int)(contours.size() - 1); idx >= 0; --idx)
    {
        int parentIdx = hierarchy[idx][3];
        if (hierarchy[idx][2] != -1 || parentIdx == -1)  // holes only (no child contours and with parent)
            continue;
        const std::vector<Point>& contour = contours[idx];

        Rect contour_rect = boundingRect(contour);
        if (contour_rect.area() < min_area)
            continue; (*Skip contours and rect with less than the minimum area.*)

        std::vector<Point> approx_contour = contour;

        const int min_approx_level = 1, max_approx_level = MAX_CONTOUR_APPROX; (MAx contour approx is 7)
        for (int approx_level = min_approx_level; approx_contour.size() > 4 && approx_level <= max_approx_level; approx_level++ )
        {
            approxPolyDP(approx_contour, approx_contour, (float)approx_level, true);
        }

        // reject non-quadrangles
        if (approx_contour.size() != 4)
            continue;
        if (!cv::isContourConvex(approx_contour))
            continue;

        cv::Point pt[4];
        for (int i = 0; i < 4; ++i)
            pt[i] = approx_contour[i];
        CV_LOG_VERBOSE(NULL, 9, "... contours(" << contour_quads.size() << " added):" << pt[0] << " " << pt[1] << " " << pt[2] << " " << pt[3]);

        if (filterQuads)
        {
            double p = cv::arcLength(approx_contour, true);
            double area = cv::contourArea(approx_contour, false);

            double d1 = sqrt(normL2Sqr<double>(pt[0] - pt[2]));
            double d2 = sqrt(normL2Sqr<double>(pt[1] - pt[3]));

            // philipg.  Only accept those quadrangles which are more square
            // than rectangular and which are big enough
            double d3 = sqrt(normL2Sqr<double>(pt[0] - pt[1]));
            double d4 = sqrt(normL2Sqr<double>(pt[1] - pt[2]));
            if (!(d3*4 > d4 && d4*4 > d3 && d3*d4 < area*1.5 && area > min_area &&
                d1 >= 0.15 * p && d2 >= 0.15 * p))
                continue;
        }

        contour_child_counter[parentIdx]++;
        if (boardIdx != parentIdx && (boardIdx < 0 || contour_child_counter[boardIdx] < contour_child_counter[parentIdx]))
            boardIdx = parentIdx;

        contour_quads.emplace_back(pt, parentIdx);
    }

    size_t total = contour_quads.size();
    size_t max_quad_buf_size = std::max((size_t)2, total * 3);
    all_quads.allocate(max_quad_buf_size);
    all_corners.allocate(max_quad_buf_size * 4);

    // Create array of quads structures
    for (size_t idx = 0; idx < total; ++idx)
    {
        QuadCountour& qc = contour_quads[idx];
        if (filterQuads && qc.parent_contour != boardIdx)
            continue;

        int quad_idx = quad_count++;
        ChessBoardQuad& q = all_quads[quad_idx];

        // reset group ID
        q = ChessBoardQuad();
        for (int i = 0; i < 4; ++i)
        {
            Point2f pt(qc.pt[i]);
            ChessBoardCorner& corner = all_corners[quad_idx * 4 + i];

            corner = ChessBoardCorner(pt);
            q.corners[i] = &corner;
        }
        q.edge_sqr_len = FLT_MAX;
        for (int i = 0; i < 4; ++i)
        {
            float sqr_d = normL2Sqr<float>(q.corners[i]->pt - q.corners[(i+1)&3]->pt);
            q.edge_sqr_len = std::min(q.edge_sqr_len, sqr_d);
        }

        const int edge_len_compensation = 2 * dilations;
        q.edge_sqr_len += 2 * sqrt(q.edge_sqr_len) * edge_len_compensation + edge_len_compensation * edge_len_compensation;
    }

    all_quads_count = quad_count;

    CV_LOG_VERBOSE(NULL, 3, "Total quad contours: " << total);
    CV_LOG_VERBOSE(NULL, 3, "max_quad_buf_size=" << max_quad_buf_size);
    CV_LOG_VERBOSE(NULL, 3, "filtered quad_count=" << quad_count);
} *)

