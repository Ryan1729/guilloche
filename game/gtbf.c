#define STB_IMAGE_IMPLEMENTATION
#include "../include/stb_image.h"

#define u8 unsigned char

int write_bin(char* dest_path, u8* bytes, int byte_count) {
    FILE *write_ptr = fopen(dest_path,"wb");  // w for write, b for binary

    if (!write_ptr) {
        return 0;
    }

    return fwrite(bytes, byte_count, 1, write_ptr);
}

void write_template_bins(
    char* template_png_path,
    char* size_bin_dest_path,
    char* pixels_bin_dest_path
) {
    u8* pixels;
    int w,h;

    pixels = stbi_load(template_png_path, &w, &h, 0, 3);
    if (pixels == 0) {
        fprintf(stderr, "Couldn't open '%s'\n", template_png_path);
        exit(1);
    }

    u8 size[] = {
        w & 0xff, (w >> 8) & 0xff, (w >> 16) & 0xff, (w >> 24) & 0xff,
        h & 0xff, (h >> 8) & 0xff, (h >> 16) & 0xff, (h >> 24) & 0xff,
    };

    int size_wrote_count = write_bin(size_bin_dest_path, size, 8);
    if (size_wrote_count != 1) {
        fprintf(stderr, "Couldn't write '%s'\n", size_bin_dest_path);
        exit(2);
    }

    int pixels_wrote_count = write_bin(pixels_bin_dest_path, pixels, w * h * 3);
    if (pixels_wrote_count != 1) {
        fprintf(stderr, "Couldn't write '%s'\n", pixels_bin_dest_path);
        exit(2);
    }
}

int main(int argc, char **argv)
{
    write_template_bins("t1.png", "src/t1_size.bin", "src/t1_pixels.bin");

    return 0;
}