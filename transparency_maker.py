import os
import fnmatch

from PIL import Image

image_dir=os.path.join(os.getcwd(), 'images_exp2') # File path
destination_dir=os.path.join(os.getcwd(), 'new')

image_files = [
        os.path.join(dirpath, f)
        for dirpath, dirnames, files in os.walk(image_dir)
        for f in fnmatch.filter(files, '*.jpg')]

for image_name in image_files:
    img = Image.open(image_name).convert("RGBA") # Opens images and converts it to RGBA format
    datas = img.getdata() # Gets pixels

    newData = []
    for item in datas:
        if item[0] == 255 and item[1] == 255 and item[2] == 255: # If white
            newData.append((255, 255, 255, 0)) # Convert to transparent
        else:
            newData.append(item)

    img.putdata(newData) # Replaces old pixels with new
    pre, ext = os.path.splitext(image_name) # Splits name of file and file extension
    '''base = os.path.basename(pre)
    new_image_name = os.path.join(destination_dir, base + ".png")'''
    new_image_name = pre + ".png"
    img.save(new_image_name, "PNG") # Changes file extension to .png
    print "renaming " + image_name + " to " + new_image_name