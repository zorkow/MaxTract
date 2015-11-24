// gcc ccl.c -o ccl -ltiff -lm

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "tiffio.h"


#define max_int 2147483647

/*
 a linked list node 
 it represents a pixel (x,y) with a link to another pixel
 so it saves a run of pixels
*/
struct pixel
{
    uint x,y;
    struct pixel *next;
};


/* bounding box coordinates */
struct bbox
{
    int x1,y1,x2,y2;
};

/*
 initialise a run of pixels
 the passed values of i and j will not be saved, this will only initialise the linked list
*/
struct pixel* init_pixel(struct pixel* head, int i, int j) {
  //struct pixel* head = NULL;
  head = malloc(sizeof(struct pixel)); // allocate node in the heap
  head->x = i; // setup first parameter
  head->y = j; // setup second parameter
  head->next = NULL; // note: pointer assignment rule

}

/* 
push pixel into the linked list
*/
void push(struct pixel** headRef, int i, int j) {   
   struct pixel* newNode = malloc(sizeof(struct pixel));
   newNode->x = i;
   newNode->y = j;
   newNode->next = *headRef; // The '*' to dereferences back to the real head
  *headRef = newNode; // ditto
//  free(newNode);
}


/*
Given a linked list head pointer, compute
and return the number of nodes in the list.
*/
int length(struct pixel* head) {
  struct pixel* current = head;
  int count = 0;
  while (current != NULL) {
    count++;
    current = current->next;
  }
  free(current);
  return count;
}


/*
Given a linked list head pointer, print out contents of list
*/
void print_list (struct pixel* head) {
  struct pixel* current = head;
  //int count = 0;
  while (current != NULL) {
  //count++;
   //if (current->x == 0 && current->y == 0) printf("WAAAAAK\n");
  printf("%d,%d ", current->x , current->y );
  current = current->next;
  }
  printf("\n");
  free(current);
  //return count;
}

/*
 loop through a list of pixels and find xmin, ymin, xmax and ymax
 the list of pixels has pixels of the same CC
*/
struct bbox* find_bbox (struct pixel* head) {
  struct pixel* current = head;
  int i1=max_int, j1=max_int,i2=0, j2=0 ;
  while (current != NULL) {  
    if(current->x < i1) i1 = current->x;
    if(current->y < j1) j1 = current->y;
    if(current->x > i2) i2 = current->x;
    if(current->y > j2) j2 = current->y;
    //printf("%d,%d ", current->x , current->y );
    current = current->next;
  }
  //printf("\n");
  struct bbox* b = malloc(sizeof(struct bbox));;
  b->x1= i1;
  b->y1= j1;
  b->x2= i2;
  b->y2= j2;
  //printf("bbox: (%d,%d),(%d,%d)\n", i1 , j1, (i2+1), (j2+1) );
  //bbox: (299,249),(308,277)
  //bbox: (1269,3160),(1281,3187)
  return b;
}

// small function to strip off path and return filename only
char *get_basename(char *path)
{
    //forward slash assuming it's UNIX based environment
    char *base = strrchr(path, '/');
    return base ? base+1 : path;
}

main(int argc, char* argv[])
{
  //START;
  
  // the required page number in a multipage tiff, use 0 for first page
  int dir_num;
  dir_num = atoi(argv[2]);
  //time_t start, end;
  double duration;
  TIFF* tif = TIFFOpen(argv[1], "r");
  //get number of pages (Directories) in a tiff file
    int dir_count;
    dir_count = TIFFNumberOfDirectories(tif);
    if(dir_num < 0 || dir_num > (dir_count-1)){
       printf("Error: invalid directory number\n");
       exit -1;
    }
    else{
    
   if (tif) {
    uint32 w, h;
    size_t npixels;
    uint32* raster;
    // set the required page (directory)
    // change to the requested directory and read its contents with TIFFReadDirectory
    TIFFSetDirectory(tif,dir_num);
      
    TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &w);
    TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &h);
    npixels = w * h;

    int r,v;
    //Foreground and Background pixel values
    int F = 1, B = 0;
    size_t c_h, c_w;
    raster = (uint32*) _TIFFmalloc(npixels * sizeof (uint32));
    // image matrix with added boundary pixels
    int** I = malloc((h+2) * sizeof(int*));    // allocate the rows
    for ( r = 0; r < (h+2); ++r)
     {
       I[r] = malloc((w+2) * sizeof(int));    // allocate the columns
       //for ( cx = 0; cx < COLS; ++cx)
        // I[r][cx] = 0;
     }
     
     // an array of pixel runs (hashtable of pixel runs)
     // each run is a linked list of pixels
     struct pixel **runs = malloc((h*w/4) * sizeof(struct pixel));
     //initialise arrays and linked list
     for ( v = 0; v < (h*w/4); ++v)
     {
       //C[r] = r;    // initialise labels 
       init_pixel(runs[v], 99,99);
     }
     // arrays to save classes and their mappings
     //uint32* C = malloc((ROWS*COLS/2) * sizeof(uint32*)); 
     uint32* rl_table = malloc((h*w/4) * sizeof(uint32*)); 
     uint32* n_label = malloc((h*w/4) * sizeof(uint32*)); 
     uint32* t_label = malloc((h*w/4) * sizeof(uint32*));
     for ( v = 0; v < (h*w/4); ++v)
     {
       //C[r] = r;    
       // initialise classes
       rl_table[v] = v;  
       n_label[v] = -1;
       t_label[v] = v;      
     }
    
    // load image into matrix of F and B pixels (1's and 0's)
    if (raster != NULL) {
      if (TIFFReadRGBAImage(tif, w, h, raster, 0)) {          
        for(c_h=0;c_h<h;c_h++) {
           for(c_w=0;c_w<w;c_w++) {
              v = raster[(w*h)-((c_h*w)+(w-c_w))]%256;
              if (v == 0)
                I[c_h+1][c_w+1] = F;
              else
                I[c_h+1][c_w+1] = B;
           }          
        }               
      }
      _TIFFfree(raster);
    }
    
    /*
     c2  c3  c4
     c1  x 
    */
  

  // FIRST SCAN
  int i,j,k;
  int NewLabel=5; 

 int c1,c2,c3,c4,uu,vv,ii;
  for(c_h=1;c_h<(h-1);c_h++)
    for(c_w=1;c_w<(w-1);c_w++){ // for COLS
      if (I[c_h][c_w] == F) 
	{ 
	    c3 = I[c_h-1][c_w] ;
	    if (c3 != B) 
	    {
		I[c_h][c_w] = c3;
	    }
	    else // else1
	    {
	       c4 = I[c_h-1][c_w+1] ;
	       c1 = I[c_h][c_w-1] ;
	      if (c1 != B) 
	      {
		 I[c_h][c_w] = c1;
		 if (c4 != B && c4 != c1) 
		 {		  
		  //printf ("(%d,%d);", c1, c4);
		  //(* resolve c2 c4 *)	
		  uu = rl_table[c1]; vv = rl_table[c4];
		   if(uu<vv){
		        ii = vv;                      
                        while(ii != -1){
                          rl_table[ii] = uu;
                          ii = n_label[ii];
                        }                      
                        n_label[t_label[uu]] = vv;
                        t_label[uu] = t_label[vv];
		   }
		   else{
		      if(vv<uu){
		        ii = uu;                      
                        while(ii != -1){
                          rl_table[ii] = vv;
                          ii = n_label[ii];
                        }                      
                        n_label[t_label[vv]] = uu;
                        t_label[vv] = t_label[uu];
		      }
		   }		                     
		 }
	      }
	      else // else2
	      {
		 c2 = I[c_h-1][c_w-1] ;
		  if (c2 != B) 
		  {
		     I[c_h][c_w] = c2;
		      if (c4 != B && c4 != c2) 
		      {				               
			 //printf("(%d,%d);\n", c2, c4); 
		  	 //(* resolve c2 c4 *)
		 	 uu = rl_table[c2]; vv = rl_table[c4];
		         if(uu<vv){
	     	            ii = vv;                      
                            while(ii != -1){
                             rl_table[ii] = uu;
                             ii = n_label[ii];
                            }                      
                            n_label[t_label[uu]] = vv;
                            t_label[uu] = t_label[vv];
		         }
		         else{
		             if(vv<uu){
		                ii = uu;                      
                                while(ii != -1){
                                   rl_table[ii] = vv;
                                   ii = n_label[ii];
                                }                      
                               n_label[t_label[vv]] = uu;
                               t_label[vv] = t_label[uu];
		             }
		         }
			 
		     }
		  }
		  else 
		    if (c4 != B) 
		    {
			I[c_h][c_w] = c4;
		    }
		    else
		    {
			I[c_h][c_w] = NewLabel;
			NewLabel=NewLabel+1;
		    }
	       }// else2
	    }// else1
	}
  } // end for COLS
  
  // SECOND SCAN
  for(c_h=0;c_h<h;c_h++)
    for(c_w=0;c_w<w;c_w++) // for COLS
        if (I[c_h][c_w] != B)
          I[c_h][c_w] = rl_table[I[c_h][c_w]];
     
   
   
//get linked lists of pixels with the same class
//i.e. pixels that belong to the same CC
 for(c_h=0;c_h<h;c_h++){
   for(c_w=0;c_w<w;c_w++){
     if (I[c_h][c_w] != B)
     {       
       push(&runs[I[c_h][c_w]], c_w,c_h);
     }
  }
 }

 //now open a file, find coords of each CC and save them
 // char *base = get_basename(argv[1]);// = "filename";  
 char *base = argv[1];
  // find location of last '.' in filename to chop extension
  char *pos = strrchr (base, '.');
  int pos1 = pos ? (pos - base ) : -1;
  
  if(pos1 != -1){
    base[pos1] = 0; // remove file extension
    //printf("%d, %s\n",pos1, base);
  }

  //I'm assuming length of filename doesn't exceed 50 chars
  //I'll deal with this later
  char filename[256];
  
  
  FILE *file;
  //fname1[strlen(fname1) - 4] = 0; // remove file extension
  
  sprintf(filename, "%s-%d.json", base, dir_num);  
  
  //printf("%s\n", filename);
  
  file = fopen(filename,"w+");
  fprintf(file,"{\n");
  fprintf(file,"  \"SrcImage\": \"\",\n"); 
  fprintf(file,"  \"Page\": %d,\n",dir_num);
  fprintf(file,"  \"PageWidth\": %d,\n", w);
  fprintf(file,"  \"PageHeight\": %d,\n",h);
  fprintf(file,"  \"ClipX\": %d,\n",0);
  fprintf(file,"  \"ClipY\": %d,\n",0);
  fprintf(file,"  \"ClipWidth\": %d,\n", w);
  fprintf(file,"  \"ClipHeight\": %d,\n",h);  
  fprintf(file,"  \"ClipImage\": \"\",\n");
  fprintf(file,"  \"glyphs\": [\n");
  
  //count++;
  //file = fopen("file.txt","w+");
  int len;
   for ( v = 0; v < (h*w/4); ++v)
     {
       len = length(runs[v]); 
       if (len > 0 ){
         //printf("run ID: %d, run length %d\n", r, len );
         struct bbox* b = find_bbox (runs[v]);
         //printf("bbox: (%d,%d),(%d,%d)\n", b->x1, b->y1, b->x2, b->y2 );   
         fprintf(file,"    { \"x\" : %d, \"y\" : %d, \"w\" : %d, \"h\" : %d }",(b->x1-1),(b->y1-1), (b->x2 - b->x1 + 1), (b->y2 - b->y1 + 1)); 
	 fprintf(file, ",\n");
         free(runs[v]);// = NULL;
         free(b);
       }
    }
    // trick by Volker to remove comma after last glyph	 
   fseek(file, -( 2*(int)sizeof(char) ), SEEK_CUR);
   fprintf(file,"\n  ]\n");
   fprintf(file,"}\n");
   // close file
   fclose(file); 
   
   //printf("Generated: %s\n",filename);
   //manually free space allocated to various arrays and matrices
     for (v = 0; v < h; ++v)
     {
       free(I[v]);    // this frees the columns
     }
     free(I);    // this frees the rows
     free(runs);
     //free(C);
     free(rl_table);
     free(n_label);
     free(t_label);
 
    TIFFClose(tif);
  }//end if tif
  } //end else 
  //STOP;
  //PRINTTIME;
     
}
