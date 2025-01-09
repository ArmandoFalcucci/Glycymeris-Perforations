# Specify the full path to your folder
folder_path <- "outlines-exp"

# Get a list of all .txt files in the folder
files <- list.files(path = folder_path, pattern = "\\.txt$", full.names = TRUE)

# Rename each file by adding the prefix "EXP_" to its current name
for (file in files) {
  # Get the base name of the file (without the full path)
  base_name <- basename(file)
  # Create the new file name with the "EXP_" prefix
  new_name <- file.path(folder_path, paste0("EXP_", base_name))
  # Rename the file
  file.rename(file, new_name)
}

# Check if renaming was successful
print(list.files(path = folder_path))


## arch

# Specify the full path to your folder
folder_path <- "outlines-archaeo"

# Get a list of all .txt files in the folder
files <- list.files(path = folder_path, pattern = "\\.txt$", full.names = TRUE)

# Rename each file by adding the prefix "EXP_" to its current name
for (file in files) {
  # Get the base name of the file (without the full path)
  base_name <- basename(file)
  # Create the new file name with the "EXP_" prefix
  new_name <- file.path(folder_path, paste0("ARCH_", base_name))
  # Rename the file
  file.rename(file, new_name)
}

# Check if renaming was successful
print(list.files(path = folder_path))


