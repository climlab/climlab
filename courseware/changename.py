# ---
# jupyter:
#   jupytext:
#     cell_metadata_filter: -all
#     formats: ipynb,md,py:percent
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.8.0
#   kernelspec:
#     display_name: Python 3
#     language: python
#     name: python3
# ---

# %% [markdown]
#
# ```{code-cell}
# from pathlib import Path
# all_files = Path().glob("*ipynb")
# for old_name in all_files:
#     new_name=str(old_name).replace(' ','_')
#     print(new_name)
#     old_name.rename(new_name)
# ```
