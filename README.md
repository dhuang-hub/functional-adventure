# functional-adventure

Header Note: I caught a non-fatal game state message bug late, and it was too irksome to leave unfixed. Grade my latest non-late submission.


Name: David Huang
Date: June 10, 2020

Acknowledgements:
Thank you Matt, Robert, and graders for an excellent quarter despite all of the unexpected challenges we're all facing at this time, y'all rock.


Newly Implemented Features:
- Entirely new map - Sorry! I'm the bad apple who didn't read the prompt carefully before coding. But I hope you find this map amusing, because... you'll see.
- "Help" Command - Type "help" any time
- "LookAt" Command - Type "look at 'things...'" to get details and object descriptions printed out. 
- "Use" Command - Type "use 'things...'" to interact with 'Items'
    - I modified the Items record data type to include a 'use' flag to capture whether or not items can be used.
    - The primary difference between the two is that Items that cannot be used will simply return the item description, similar to the 'look at' command. Whereas, items that can be used, have custom-defined 'use' functions and behaviors.
- Stairs - I implemented stairs in this new map. They are actually typed as 'Items' so any of the above commands involving 'Items' will work. Additionally, my implementation is not the most elegant, there are two stair items: 'stairsup' and 'stairsdown'.
- Doors - I implemented a few doors, and they are also typed as 'Items' than a player can interact with pre-exisiting and new 'Item' commands.
    - Logic gates: Some doors require the player to obtain and carry certain items in the inventory before they'll properly 'open'. 'Using' a door is essentially a 'move' of the player to the room on the other side.
- Mouse Traps - Be careful with play, dropping certain items in certain areas may prevent you from returning.


Other Thoughts and Comments:
There are many ways this game could be improved in implementation. But if I had more time, I would start with reworking the parsing. My current implementation rules that objects must be parsed as a string of continuous alpha and lower-case characters hence... ugly, painful-to-read, annoying-to-mistype, names of certain `ItemNames`. In short, better parsing, and smarter Data Typing to make this game implementation less 'hacky'. But I hope you find some joy in this. I certainly enjoyed designing this game, and more importantly, I thoroughly enjoyed this course very much. I've learned a lot and it certainly has opened my eyes to more things.

Thanks so much!





~~~~~~~~~~~~~~~~* SPOILER ALERT *~~~~~~~~~~~~~~~~~~~~

Solution Run Below:

south and west and take book and use book and drop book and east and south and use stairsup and east and north and use computer and take badge and south and west and use stairsup and use stairsup and use floorthreedoor and north and north and take key and north and south and west and use stairsdown and use stairsdown and use stairsdown and use basementdoor and take knowledge and west and use stairsup and east and use maindoor



Explanation of Solution:

Goal:
Escape this building, which is modeled after Crerar Library. Player begins with a measly `maxWeight` capacity of 10.

Things player must accomplish:
1. Take the 'book' in the Basement Office
2. Use the 'book' from the Basement Office -> in the background, increases player's `maxWeight` +250
3. Take the 'badge' from the Floor 1 Lab
4. Use the 'computer' from the Floor 1 Lab -> in the background, increases player's `maxWeight` +250
5. Enter Floor 3 of the building w/ 'badge' in inventory.
6. Take 'key' from Floor 3 Gerry's office.
7. Head down to Basement and open the Basement Door with 'key' inventoried to find Matt rockin' out mid-lecture.
8. Take 'knowledge'. Note: 'knowledge' is HEAVY, which is why a player must both use the resources found in both 'book' and 'computer', before taking some HEAVY 'knowledge'.
9. Leave out the Main Door on Floor 1 with 'knowledge'.