
for f in NAARC/MY_SRC/4.2.2/*; do 
	a=$(echo $f | sed "s|NAARC/MY_SRC/4.2.2/||"); 
	loc=$(find nemo_4.2.2/src -name "$a")
	echo $a >> output_test.txt
	diff NAARC/MY_SRC/4.2.2/$a $loc >> output_test.txt

	echo '' >> output_test.txt
	echo '' >> output_test.txt
	echo '' >> output_test.txt
	echo '' >> output_test.txt
	echo '' >> output_test.txt
	echo '' >> output_test.txt
	echo '' >> output_test.txt
	echo '' >> output_test.txt
	echo '' >> output_test.txt
	echo '' >> output_test.txt
	echo '' >> output_test.txt
	echo '' >> output_test.txt
	echo '' >> output_test.txt
	echo '' >> output_test.txt
done
