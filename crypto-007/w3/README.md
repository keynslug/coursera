Question 1
==========

Suppose a web site hosts large video file _F_ that anyone can download. Browsers who download the file need to make sure the file is authentic before displaying the content to the user. One approach is to have the web site hash the contents of _F_ using a collision resistant hash and then distribute the resulting short hash value _h = H(F)_ to users via some authenticated channel (later on we will use digital signatures for this). Browsers would download the entire file _F_, check that _H(F)_ is equal to the authentic hash value h and if so, display the video to the user.

Unfortunately, this means that the video will only begin playing after the *entire* file _F_ has been downloaded. Our goal in this project is to build a file authentication system that lets browsers authenticate and play video chunks as they are downloaded without having to wait for the entire file.

Instead of computing a hash of the entire file, the web site breaks the file into 1KB blocks (1024 bytes). It computes the hash of the last block and appends the value to the second to last block. It then computes the hash of this augmented second to last block and appends the resulting hash to the third block from the end. This process continues from the last block to the first as in the following diagram:

![Hashing scheme](https://d396qusza40orc.cloudfront.net/crypto/images%2Fpp3-fig.jpg)

The final hash value _h<sub>0</sub>_ – a hash of the first block with its appended hash – is distributed to users via the authenticated channel as above.

Now, a browser downloads the file _F_ one block at a time, where each block includes the appended hash value from the diagram above. When the first block _(B<sub>0</sub> || h<sub>1</sub>)_ is received the browser checks that _H(B<sub>0</sub> || h<sub>1</sub>)_ is equal to _h<sub>0</sub>_ and if so it begins playing the first video block. When the second block _(B<sub>1</sub> || h<sub>2</sub>)_ is received the browser checks that _H(B<sub>1</sub> || h<sub>2</sub>)_ is equal to _h<sub>1</sub>_ and if so it plays this second block. This process continues until the very last block. This way each block is authenticated and played as it is received and there is no need to wait until the entire file is downloaded.

It is not difficult to argue that if the hash function _H_ is collision resistant then an attacker cannot modify any of the video blocks without being detected by the browser. Indeed, since _h<sub>0</sub> = H(B<sub>0</sub> || h<sub>1</sub>)_ an attacker cannot find a pair _(B′<sub>0</sub>, h′<sub>1</sub>) ≠ (B<sub>0</sub>, h<sub>1</sub>)_ such that _h<sub>0</sub> = H(B<sub>0</sub> || h<sub>1</sub>)_ since this would break collision resistance of _H_. Therefore after the first hash check the browser is convinced that both _B<sub>0</sub>_ and _h<sub>1</sub>_ are authentic. Exactly the same argument proves that after the second hash check the browser is convinced that both _B<sub>1</sub>_ and _h<sub>2</sub>_ are authentic, and so on for the remaining blocks.

In this project we will be using **SHA256** as the hash function. For an implementation of **SHA256** use an existing crypto library such as **PyCrypto** (Python), **Crypto++** (C++), or any other. When appending the hash value to each block, please append it as binary data, that is, as 32 unencoded bytes (which is 256 bits). If the file size is not a multiple of 1KB then the very last block will be shorter than 1KB, but all other blocks will be exactly 1KB.

Your task is to write code to compute the hash _h<sub>0</sub>_ of a given file _F_ and to verify blocks of _F_ as they are received by the client. In the box below please enter the (hex encoded) hash _h<sub>0</sub>_ for [this video file](https://class.coursera.org/crypto-007/lecture/download.mp4?lecture_id=27).

You can check your code by using it to hash a different file. In particular, the hex encoded _h<sub>0</sub>_ for [this video file](https://class.coursera.org/crypto-007/lecture/download.mp4?lecture_id=28) is:

```
03c08f4ee0b576fe319338139c045c89c3e8e9409633bea29442e21425006ea8
```
