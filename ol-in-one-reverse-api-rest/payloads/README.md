## COBOL to EBCDIC Converter

This script extracts field definitions from a COBOL source file, prompts the user for corresponding values, and generates an EBCDIC buffer file based on the input. It is designed to help you efficiently transform COBOL data structures into binary files encoded in EBCDIC.

---

## Features

- Extracts `BODY-PARAMS` section within `HTTP-REQUEST` from COBOL's `WORKING-STORAGE SECTION`.
- Supports alphanumeric (X) and numeric (9) data types.
- Prompts users to enter values for extracted fields.
- Converts the input values to EBCDIC encoding.
- Generates a binary file as output.

---

## Requirements

- Python 3.6 or higher

### Python Modules Used

- `re` (Regular Expressions)
- `codecs` (Encoding)
- `argparse` (Command-line Argument Parsing)
- `os` (File Path Handling)

---

## Setup and Usage

### 1. Run the Script

#### Syntax:

```bash
python cobol_to_ebcdic.py <cobol_file> [--output <output_file>]
```

#### Arguments:

- `cobol_file`: The path to the COBOL source file you want to process.
- `--output` or `-o`: (Optional) The name of the output binary file. If omitted, the script will default to a file named `<input_filename>.bin`.

#### Example:

```bash
python cobol_to_ebcdic.py sample.cbl --output output.bin
```

### 2. Input Values

The script will extract the structure from the COBOL file and list all fields found. It will then prompt you to input values for each field. The expected input type and length will be displayed for each field.

---

## Output

- A binary file encoded in EBCDIC format will be created at the specified output path.

---

## Script Workflow

### 1. Parsing the COBOL File
- The script reads the COBOL source file and searches for the `WORKING-STORAGE SECTION`.
- Within this section, it identifies the `HTTP-REQUEST` and `BODY-PARAMS` sub-sections.
- Fields defined with `05` or `07` level numbers, `PIC X` or `PIC 9` types, and specified lengths are extracted.

### 2. Prompting for User Input
- For each extracted field, the script prompts the user to input a value.
- The value is validated to match the expected type and truncated or padded to match the required length.

### 3. Generating EBCDIC Buffer
- The collected values are converted to EBCDIC encoding using the `cp037` codepage.
- The encoded data is saved as a binary file.

---

## Error Handling

- **Missing `WORKING-STORAGE SECTION`:** If the section is not found, the script raises an error.
- **Missing `HTTP-REQUEST` or `BODY-PARAMS`:** The script informs the user of missing subsections and halts execution.
- **Invalid User Input:** The script re-prompts until valid input is provided for each field.
- **File Not Found:** If the specified COBOL file cannot be located, an error message is displayed.
- **Unexpected Errors:** Any unforeseen issues are reported with a detailed error message.

---

## Example COBOL File

```cobol
WORKING-STORAGE SECTION.
01 HTTP-REQUEST.
   03 BODY-PARAMS.
      05 FIELD1 PIC X(10).
      05 FIELD2 PIC 9(5).
```

### Sample Execution

**Input:**

```bash
python cobol_to_ebcdic.py sample.cbl --output output.bin
```

**Output:**

```
Found fields:
- FIELD1 (Type: X, Length: 10)
- FIELD2 (Type: 9, Length: 5)

Please provide values for the following fields:
Enter value for FIELD1 (max length 10): hello
Enter value for FIELD2 (max length 5): 123

EBCDIC buffer written to output.bin
```

---

## License

This project is licensed under the MIT License.


