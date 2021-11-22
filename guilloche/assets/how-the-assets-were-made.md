# How the assets were made

## Initial tiles

For the inital template I wanted arrow tiles and a a little character to move around the screen. [This CC0 tileset by surt/vk](https://opengameart.org/content/roblocks) fit the bill. So I clipped some of the tiles from there.

## Guilloche specific tiles

I wanted a way to render more or less flat colours, but since the template already has sprite rendering set up, I might as well just use sprites. I also figured I might as well try to get some art that I might actually use later. So I grabbed [this CC0 tileset by surt/vk/Sharm](https://opengameart.org/content/simple-broad-purpose-tileset) and pulled some flat-ish tiles out for now. I used the select-by-colour tool in GIMP to remove the #5a5268 background and replace it with transparency, (after adding an alpha channel). In order to make the new tiles fit, I increased the canvas size to the next highest power of two in the x direction that would fit, (512 pixels).

I ended up making the vertical order of the dark brown wall tiles match that of some of the other tiles, to make the sprite look up code slightly simpler.

Since I was already using [this CC0 tileset by surt/vk/Sharm](https://opengameart.org/content/simple-broad-purpose-tileset), I pulled 128 item sprites from there, and manually packed them into a 16 by 8 tile rectangle. The sprites were selected based on whether I thought I could write an at least slightly intersting text description and/or conversation with the NPC about what they wanted, in case I decide I want that. For example, I think I can come up with a four types of soda to describe the four cans as being, but coming up with four different types of coffee seems harder. If I make the red cup the normal coffe, the orange cup decaf, and describe the gray cup on a saucer as Earl Grey tea, and the green on as green tea, then that seems sufficently interesting.
