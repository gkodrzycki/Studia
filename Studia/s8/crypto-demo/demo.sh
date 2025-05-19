az keyvault secret list --vault-name $KEY_VAULT_NAME

az keyvault secret set --vault-name $KEY_VAULT_NAME --name my-secret --value "superSecret123"

az keyvault secret show --vault-name $KEY_VAULT_NAME --name my-secret

az keyvault secret show --vault-name $KEY_VAULT_NAME --name my-secret --query value -o tsv

az keyvault secret delete --vault-name $KEY_VAULT_NAME --name my-secret

az keyvault secret recover --vault-name $KEY_VAULT_NAME --name my-secret

az keyvault secret purge --vault-name $KEY_VAULT_NAME --name my-secret
