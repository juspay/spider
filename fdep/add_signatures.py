import json
import os

RECURSIVE_ROOT = "./"

signatures = dict()

for current_dir_path, current_subdirs, current_files in os.walk(RECURSIVE_ROOT):
    for aFile in current_files:
        if aFile.endswith(".hs.missing.signatures.json"):
            txt_file_path = str(os.path.join(current_dir_path, aFile))
            print(txt_file_path)
            with open(txt_file_path,'r') as f:
                d = json.load(f)
            for (k,v) in d.items():
                flle_path = k.split(":")[0]
                line_no = k.split(":")[1]
                col_start = k.split(":")[2].split("-")[0]
                if signatures.get(flle_path) == None:
                    signatures[flle_path] = [(int(line_no),int(col_start),v.replace("\n",""))]
                signatures[flle_path].append((int(line_no),int(col_start),v.replace("\n","")))

print(len(signatures))

for (file,sigs) in signatures.items():
    cnt = 0
    print(file)
    with open(file,'r') as f:
        content = f.readlines()
    sigs.sort()
    for (line_no,col_no,sig) in sigs:
        print(line_no)
        line_no += (cnt - 1)
        spaces = " " * (col_no - 1)
        if content[line_no - 1].strip() != sig.strip():
            content.insert(line_no,spaces + sig + "\n")
            cnt += 1
    os.remove(file)
    with open(file,'w') as f:
        f.writelines(content)