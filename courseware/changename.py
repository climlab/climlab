from pathlib import Path
all_files = Path().glob("*ipynb")
for old_name in all_files:
    new_name=str(old_name).replace(' ','_')
    print(new_name)
    old_name.rename(new_name)
    
