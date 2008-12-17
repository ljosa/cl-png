#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <png.h>

png_bytepp read_image(unsigned int *width, unsigned int *height)
{
     FILE *fp = fopen("Fall.png", "rb");
     assert(fp);
     png_structp png_ptr = png_create_read_struct
          (PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
     assert(png_ptr);
     png_infop info_ptr = png_create_info_struct(png_ptr);
     assert(info_ptr);
     png_infop end_info = png_create_info_struct(png_ptr);
     assert(end_info);
     png_init_io(png_ptr, fp);
     int png_transforms = PNG_TRANSFORM_STRIP_16 |
          PNG_TRANSFORM_STRIP_ALPHA |
          PNG_TRANSFORM_PACKING;
     png_read_png(png_ptr, info_ptr, png_transforms, NULL);
     png_bytepp row_pointers = png_get_rows(png_ptr, info_ptr);
     png_uint_32 w, h;
     int bit_depth, color_type;
     png_get_IHDR(png_ptr, info_ptr, &w, &h, &bit_depth, &color_type,
                  NULL, NULL, NULL);
     printf("%u x %u\n", w, h);
     if (width) *width = w;
     if (height) *height = h;
     return row_pointers;
#if 0
     png_bytep *blob = malloc(w * h * 3);
     assert(blob);
     int i;
     for (i = 0; i < h; i++)
          memcpy(blob + i * w * 3, row_pointers[i], w * 3);
     png_destroy_read_struct(&png_ptr, &info_ptr, &end_info);
     fclose(fp);
     return blob;
#endif
}

void write_image(png_bytepp rows, unsigned int width, unsigned int height)
{
     FILE *fp = fopen("/dev/null", "wb");
     assert(fp);
     png_structp png_ptr = png_create_write_struct
          (PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
     assert(png_ptr);
     png_infop info_ptr = png_create_info_struct(png_ptr);
     assert(info_ptr);
     png_init_io(png_ptr, fp);
     png_set_IHDR(png_ptr, info_ptr, width, height, 8, PNG_COLOR_TYPE_RGB,
                  PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT,
                  PNG_FILTER_TYPE_DEFAULT);
     png_set_rows(png_ptr, info_ptr, rows);
     int png_transforms = PNG_TRANSFORM_IDENTITY;
     png_write_png(png_ptr, info_ptr, png_transforms, NULL);
     png_destroy_write_struct(&png_ptr, &info_ptr);
     fclose(fp);
}

int main(int argc, char *argv[])
{
     int times;
     if (argc == 2)
          times = atoi(argv[1]);
     else
          times = 1;
     unsigned int width, height;
     png_bytepp rows = read_image(&width, &height);

     struct timeval start, end;
     gettimeofday(&start, NULL);

     int i;
     for (i = 0; i < times; i++) {
          printf("%d\n", i);
          write_image(rows, width, height);
     }

     gettimeofday(&end, NULL);
     printf("%f s\n", (end.tv_sec - start.tv_sec +
                       (end.tv_usec - start.tv_usec) / 1000000.0) / times);
     return 0;
}
