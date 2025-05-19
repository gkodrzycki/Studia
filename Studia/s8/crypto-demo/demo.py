import os
from azure.keyvault.secrets import SecretClient
from azure.identity import DefaultAzureCredential
from azure.core.exceptions import HttpResponseError

def get_client():
    key_vault_name = os.environ.get("KEY_VAULT_NAME")
    if not key_vault_name:
        print("❌ Environment variable KEY_VAULT_NAME is not set.")
        exit(1)
    kv_uri = f"https://{key_vault_name}.vault.azure.net"
    credential = DefaultAzureCredential()
    return SecretClient(vault_url=kv_uri, credential=credential)

def create_or_update_secret(client):
    name = input("🔐 Secret name: ")
    value = input("✏️ Secret value: ")
    try:
        client.set_secret(name, value)
        print("✅ Secret created/updated.")
    except HttpResponseError as e:
        print(f"❌ Error: {e.message}")

def retrieve_secret(client):
    name = input("🔍 Secret name to retrieve: ")
    try:
        secret = client.get_secret(name)
        print(f"🔓 Value: {secret.value}")
    except HttpResponseError as e:
        print(f"❌ Error: {e.message}")

def delete_secret(client):
    name = input("🗑️ Secret name to delete: ")
    try:
        poller = client.begin_delete_secret(name)
        poller.wait()
        print("✅ Secret deleted (soft delete).")
    except HttpResponseError as e:
        print(f"❌ Error: {e.message}")

def purge_secret(client):
    name = input("☠️ Secret name to purge (permanent delete): ")
    try:
        client.purge_deleted_secret(name)
        print("✅ Secret purged.")
    except HttpResponseError as e:
        print(f"❌ Error: {e.message}")

def list_secrets(client):
    print("📄 Listing secrets in vault:")
    try:
        for secret in client.list_properties_of_secrets():
            print(f"• {secret.name}")
    except HttpResponseError as e:
        print(f"❌ Error: {e.message}")

def menu():
    client = get_client()
    while True:
        print("\n===== Azure Key Vault CLI =====")
        print("1️⃣  Create or update a secret")
        print("2️⃣  Retrieve a secret")
        print("3️⃣  Delete a secret")
        print("4️⃣  Purge a secret")
        print("5️⃣  List all secrets")
        print("0️⃣  Exit")
        choice = input("Select an option: ").strip()

        if choice == "1":
            create_or_update_secret(client)
        elif choice == "2":
            retrieve_secret(client)
        elif choice == "3":
            delete_secret(client)
        elif choice == "4":
            purge_secret(client)
        elif choice == "5":
            list_secrets(client)
        elif choice == "0":
            print("👋 Exiting.")
            break
        else:
            print("❓ Invalid choice. Try again.")

if __name__ == "__main__":
    menu()
