#!/usr/bin/env python3

import argparse
import json

customers = [
    {"name": "John Doe", "country": "USA" },
    {"name": "Jane Smith", "country": "Canada"},
    {"name": "Alice Johnson", "country": "UK"},
    {"name": "Bob Brown", "country": "Australia"}
]

def filter_customers_by_country(country):
    return [customer for customer in customers if customer["country"] == country]

def load_customers_from_file(file_path):
    with open(file_path, 'r') as file:
        global customers
        customers = json.load(file)

def main(country=None, file_path=None):
    if file_path:
        load_customers_from_file(file_path)
    if country:
        filtered_customers = filter_customers_by_country(country)
    else:
        filtered_customers = customers
    for customer in filtered_customers:
        print(customer)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--country", help="Filter customers by country")
    parser.add_argument("--file", help="Path to the file containing customers")
    args = parser.parse_args()
    main(args.country, args.file)
