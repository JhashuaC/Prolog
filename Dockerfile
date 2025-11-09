# Imagen base con SWI-Prolog
FROM swipl:latest

# Establecer el directorio de trabajo
WORKDIR /app

# Copiar todos los archivos del proyecto al contenedor
COPY . .

# Exponer el puerto (Railway usa una variable PORT)
EXPOSE 8080

# Comando de inicio (Railway ejecutará esto)
CMD ["swipl", "-q", "-f", "run.pl"]
