# How the assets were made

## Initial tiles

For the inital template I wanted arrow tiles and a a little character to move around the screen. [This CC0 tileset by surt/vk](https://opengameart.org/content/roblocks) fit the bill. So I clipped some of the tiles from there.

## Guilloche specific tiles

I wanted a way to render more or less flat colours, but since the template already has sprite rendering set up, I might as well just use sprites. I also figured I might as well try to get some art that I might actually use later. So I grabbed [This CC0 tileset by surt/vk/Sharm](https://opengameart.org/content/simple-broad-purpose-tileset) and pulled some flat-ish tiles out for now. I used the select-by-colour tool in GIMP to remove the #5a5268 background and replace it with transparency, (after adding an alpha channel). In order to make the new tiles fit, I increased the canvas size to the next highest power of two in the x direction that would fit, (512 pixels).

I ended up making the vertical order of the dark brown wall tiles match that of some of the other tiles, to make the sprite look up code slightly simpler.