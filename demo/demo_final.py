"""Demo module for filtering customers."""
import argparse
from typing import List, Optional

CUSTOMERS = [
    {"name": "John Doe", "country": "USA"},
    {"name": "Jane Smith", "country": "Canada"},
    {"name": "Alice Johnson", "country": "UK"},
    {"name": "Bob Brown", "country": "Australia"},
]

def filter_customers_by_country(customers: List[dict], country: str) -> List[dict]:
    """Filter customers by country.

    Args:
        customers (List[dict]): List of customers.
        country (str): Country to filter customers by.

    Returns:
        List[dict]: List of customers in the given country.
    """
    return [customer for customer in customers if customer["country"] == country]

def main(country: Optional[str] = None) -> None:
    """Filter and print customers by country.

    Args:
        country (str, optional): Country to filter customers by.
            Defaults to None.
    """
    if country is None:
        for customer in CUSTOMERS:
            print(customer)
    else:
        filtered_customers = filter_customers_by_country(CUSTOMERS, country)
        for customer in filtered_customers:
            print(customer)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--country", help="Filter customers by country")
    args = parser.parse_args()
    main(args.country)
