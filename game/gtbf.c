#define STB_IMAGE_IMPLEMENTATION
#include "../include/stb_image.h"

#define u8 unsigned char

void write_bin(char* dest_path, u8* bytes) {
    fprintf(stderr, "write_bin TODO\n");
    exit(2);
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

    write_bin(pixels_bin_dest_path, size);
    write_bin(size_bin_dest_path, pixels);
}

int main(int argc, char **argv)
{
    write_template_bins("t1.png", "t1_size.bin", "t1_pixels.bin");

    return 0;
}