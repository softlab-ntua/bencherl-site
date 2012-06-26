#!/bin/bash

for f in `ls images/*.ps`; do
	b=`basename $f .ps`
	echo "Converting $b..."
	convert -density 150 -geometry 100% images/$b.ps images/$b.png
done


