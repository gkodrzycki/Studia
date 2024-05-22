for IMG in *.jpg
do
    convert $IMG $(basename $IMG .jpg).png
done