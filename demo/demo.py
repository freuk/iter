#!/usr/bin/env python3

import argparse
import json

customers = [
    {"name": "John Doe", "country": "USA", "currency": "USD"},
    {"name": "Jane Smith", "country": "Canada", "currency": "CAD"},
    {"name": "Alice Johnson", "country": "UK", "currency": "GBP"},
    {"name": "Bob Brown", "country": "Australia", "currency": "AUD"}
]

def filter_customers_by_country(country):
    return [customer for customer in customers if customer["country"] == country]

def filter_customers_by_currency(currency):
    return [customer for customer in customers if customer["currency"] == currency]

def load_customers_from_file(file_path):
    with open(file_path, 'r') as file:
        global customers
        customers = json.load(file)

def main(country=None, currency=None, file_path=None):
    if file_path:
        load_customers_from_file(file_path)
    if country:
        filtered_customers = filter_customers_by_country(country)
    elif currency:
        filtered_customers = filter_customers_by_currency(currency)
    else:
        filtered_customers = customers
    for customer in filtered_customers:
        print(customer)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--country", help="Filter customers by country")
    parser.add_argument("--currency", help="Filter customers by currency")
    parser.add_argument("--file", help="Path to the file containing customers")
    args = parser.parse_args()
    main(args.country, args.currency, args.file)
