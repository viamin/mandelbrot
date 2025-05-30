# mandelbrot

A learning experiment in Erlang to generate a Mandelbrot set image

To generate a mandelbrot set image, run the following command:

```sh
erlc bmp.erl && erlc mandelbrot.erl && erl -noshell -s mandelbrot start 8 800 600 -s init stop
```

This will generate a 800x600 image with 8 bits per pixel.

The image will be saved to `/tmp/mandelbrot.bmp`.

![mandelbrot](https://github.com/user-attachments/assets/03aa1070-16e9-4db7-b001-2a0d663edbcd)
