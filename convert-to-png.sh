#!/bin/bash

for f in `ls images/graphs/*.eps`; do
	b=`basename $f .eps`
	echo "Converting $b..."
	convert -density 150 -geometry 100% images/graphs/$b.eps images/graphs/$b.png
done


