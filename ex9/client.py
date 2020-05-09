import sys
import time
from lxml import html
import requests

# I'm once again asking you for your algorithms, geeksforgeeks.org
def longest_palindromic_subsequence(s): 
      
    n = len(s) 
  
    # a[i] is going to store length 
    # of longest palindromic subsequence 
    # of substring s[0..i] 
    a = [0] * n 
  
    # Pick starting point 
    for i in range(n-1, -1, -1): 
  
        back_up = 0
  
    # Pick ending points and see if s[i] 
    # increases length of longest common 
    # subsequence ending with s[j]. 
        for j in range(i, n): 
  
    # similar to 2D array L[i][j] == 1 
    # i.e., handling substrings of length 
    # one. 
            if j == i:  
                a[j] = 1 
  
    # Similar to 2D array L[i][j] = L[i+1][j-1]+2 
    # i.e., handling case when corner characters 
    # are same.  
            elif s[i] == s[j]: 
                temp = a[j] 
                a[j] = back_up + 2
                back_up = temp 
  
    # similar to 2D array L[i][j] = max(L[i][j-1], 
    # a[i+1][j]) 
            else: 
                back_up = a[j] 
                a[j] = max(a[j - 1], a[j]) 
  
    return a[n - 1] 

limit = None

count = 1
# limit = 5
we_done_here = False

params = dict()
if limit is not None:
    params['limit'] = limit

session = requests.Session()

target_url = sys.argv[1]
print(f"Issuing GET request to {target_url}.")
page = session.get(target_url, params=params)

assert(page.status_code == 200)
print("OK.")
print("Starting to play.")



# start playing
while not we_done_here:
    # parse content
    tree = html.fromstring(page.content)

    print()
    # find objective
    objective_id = "question"
    # print(f'Looking for element with id="{objective_id}".')
    objective_div = tree.get_element_by_id(objective_id)
    print(f"Round {count}, length: {len(objective_div.text)}, {objective_div.text}")

    lps = longest_palindromic_subsequence(objective_div.text)
    answer = len(objective_div.text) - lps

    print(f"Answer: {answer}")

    payload = {
        'answer': answer
        }

    # Post reply to question
    big_reveal = session.post(target_url, data=payload, params=params)

    if "Right" in big_reveal.text:
        print("Right :)")
        count += 1
    elif "Wrong" in big_reveal.text:
        print("Wrong :(")
    else:
        print("Error: Something went wrong.")

    if "Congra" in big_reveal.text:
        we_done_here = True
        print("Game finished.")
        break

    # input("Press any key to continue...")
    time.sleep(0.05)
    # Click continue
    page = session.post(target_url, params=params)
    
