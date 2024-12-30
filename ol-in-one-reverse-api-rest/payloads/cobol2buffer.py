import re
from codecs import encode
import argparse
import os


def extract_structure(cobol_content):
    """Extract the relevant structure from COBOL file content"""
    # Find the WORKING-STORAGE SECTION
    working_storage = re.search(r'WORKING-STORAGE SECTION\.(.*?)(?=\s*PROCEDURE DIVISION|\Z)', 
                              cobol_content, re.DOTALL)
    
    if not working_storage:
        raise ValueError("Could not find WORKING-STORAGE SECTION")
    
    # Find HTTP-REQUEST and BODY-PARAMS section
    section_pattern = r'01\s+HTTP-REQUEST\.(.*?)(?=01\s+|\Z)'
    body_params_pattern = r'03\s+BODY-PARAMS\.(.*?)(?=03\s+|\Z)'
    
    http_request = re.search(section_pattern, working_storage.group(1), re.DOTALL)
    if not http_request:
        raise ValueError("Could not find HTTP-REQUEST section")
    
    body_params = re.search(body_params_pattern, http_request.group(1), re.DOTALL)
    if not body_params:
        raise ValueError("Could not find BODY-PARAMS section")
    
    return body_params.group(1)

def parse_cobol_structure(cobol_content):
    """Parse COBOL structure and extract fields"""
    try:
        # First extract the relevant structure
        structure = extract_structure(cobol_content)
        
        fields = []
        # Pattern to capture field type and length
        field_pattern = r'(?:05|07)\s+(\w+(?:-\w+)*)\s+PIC\s+([X9])\((\d+)\)'
        matches = re.findall(field_pattern, structure, re.IGNORECASE)
        
        for field_name, field_type, field_length in matches:
            fields.append({
                "name": field_name,
                "type": field_type,  # X for alphanumeric, 9 for numeric
                "length": int(field_length),
            })
        return fields
    except Exception as e:
        raise ValueError(f"Error parsing COBOL structure: {str(e)}")

def prompt_user_for_values(fields):
    """Prompt user for input values based on fields"""
    values = {}
    for field in fields:
        while True:
            try:
                user_input = input(f"Enter value for {field['name']} (max length {field['length']}): ")
                if field['type'] == 'X':
                    values[field['name']] = user_input[:field['length']].ljust(field['length'])
                elif field['type'] == '9':
                    values[field['name']] = f"{float(user_input):0{field['length']}.3f}".replace('.', '')
                break
            except ValueError:
                print(f"Invalid input for field {field['name']}. Please try again.")
    return values

def generate_ebcdic_buffer(values):
    """Generate EBCDIC buffer from values"""
    ebcdic_buffer = b""
    for value in values.values():
        ebcdic_buffer += encode(value, "cp037")
    return ebcdic_buffer

def get_default_output_filename(cobol_file):
    """Generate default output filename based on input COBOL filename"""
    base_name = os.path.splitext(cobol_file)[0]  # Remove extension
    return f"{base_name}.bin"

def main():
    # Set up argument parser
    parser = argparse.ArgumentParser(description='Convert COBOL structure to EBCDIC buffer')
    parser.add_argument('cobol_file', help='Path to the COBOL source file')
    parser.add_argument('--output', '-o', default='ebcdic_output.bin',
                      help='Output file path (default: ebcdic_output.bin)')
    
    args = parser.parse_args()
    
    try:
        output_file = get_default_output_filename(args.cobol_file)
        # Read the COBOL file
        with open(args.cobol_file, 'r') as file:
            cobol_content = file.read()
        
        # Parse the COBOL structure
        fields = parse_cobol_structure(cobol_content)
        
        if not fields:
            print("No fields found in the COBOL structure.")
            return

        # Print found fields
        print("\nFound fields:")
        for field in fields:
            print(f"- {field['name']} (Type: {field['type']}, Length: {field['length']})")

        # Prompt the user for input values
        print("\nPlease provide values for the following fields:")
        user_values = prompt_user_for_values(fields)

        # Generate the EBCDIC buffer
        ebcdic_result = generate_ebcdic_buffer(user_values)

        # Write the buffer to a file
        with open(output_file, "wb") as f:
            f.write(ebcdic_result)

        print(f"\nEBCDIC buffer written to {output_file}")
        
    except FileNotFoundError:
        print(f"Error: Could not find COBOL file: {args.cobol_file}")
    except ValueError as e:
        print(f"Error: {str(e)}")
    except Exception as e:
        print(f"An unexpected error occurred: {str(e)}")

if __name__ == "__main__":
    main()
