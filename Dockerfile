FROM swipl:latest

WORKDIR /app
COPY . .

# Railway define PORT, pero exponemos por defecto 8080 por compatibilidad
ENV PORT=8080
EXPOSE 8080

CMD ["bash", "-c", "swipl -q -f run.pl"]
