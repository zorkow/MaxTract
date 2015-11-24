# Connected Component Labelling

Simple connected component labelling for multi-page TIFF images.

- The algorithm can be found [here][1]
- Equivalence resolution techinque can be found [here][2]
- This is an experimental version
- VERY FAST!
- REQUIRES libtiff
- The code is by no means optimised and needs some clean up 
- It uses 8 connectivity

[1]: http://www.sciencedirect.com/science/article/pii/S0031320308004573
[2]: http://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=04472694


## Usage

- Compile and pass two arguments, a tiff file name and and an integer
- The integer indicates the required directory number in a multipage tiff (first page is at 0)
- Use 0 for the integer if you input a single page tiff
- Works for both single/multi-page tiff
- The sample directory includes some sample tiff files, e.g., drr12.tif is a 9-page tiff

Glyph bounding box output is of the form:

    { "x" : startX, "y" : startY, "w" : width, "h" : height },


To Compile:

    make all

or

    gcc ccl.c -o ccl -ltiff -lm

To run:

    ./ccl input.tif pagenumber

Result:

    input-pagenumber.json


## Testing

The program is tested by redrawing the generated BBoxes in the input image.
In addition there is a comparison script for json files. Run with

    ./compare.py file1.json file2.json


### TODO:
1 Fix file naming



Author: Noureddin Sadawi (MON 03/12/2012)
