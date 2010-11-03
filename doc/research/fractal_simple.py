import random
import math

import numpy
import PIL.Image

#seed the random generator for repeatable output
seed = 42
rng = random.Random(seed)
dimension = 1.0

def per_pixel(value,n_per_level,x):
    p_per_pixel = (n_per_level ** dimension)/(n_per_level ** 2)
    p =p_per_pixel *(1 - value/255.)
    if x < p:
        return 0
    else:
        return 255


def expand(image,
    n_per_level=4, interpolation = PIL.Image.NEAREST):
    '''expand the image by n_per_level, adding detail
    '''
    
    #scale the image
    output = image.resize(
        (image.size[0]*n_per_level,
        image.size[1]*n_per_level),
        interpolation
        )
    pixels = output.load()
    
    for i in range(output.size[0]):
        for j in range(output.size[1]):
            x = rng.random()
            pixels[i,j] = per_pixel(pixels[i,j],n_per_level,x)

    return output

image = PIL.Image.new('L',(1,1), color = 0)

while image.size[0] < 256:
    image = expand(image)
image.show()
image.save('fractal%0.2f.png' % dimension)
