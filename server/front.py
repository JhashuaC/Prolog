import matplotlib.pyplot as plt

# Datos
familias = [
    "Musaceae", "Poaceae", "Fabaceae", "Myrtaceae", "Moraceae",
    "Anacardiaceae", "Urticaceae", "Boraginaceae", "Malpighiaceae",
    "Verbenaceae", "Arecaceae", "Cactaceae", "Lauraceae", "Burseraceae",
    "Sapotaceae", "Solanaceae", "Sapindaceae", "Bignoniaceae", "Combretaceae",
    "Bromeliaceae", "Rutaceae", "Malvaceae", "Annonaceae", "Melastomataceae",
    "Vitaceae", "Apocynaceae", "Acanthaceae"
]

valores = [
    3, 2, 3, 1, 2,
    3, 1, 1, 1,
    1, 4, 1, 1, 1,
    1, 2, 1, 1, 1,
    1, 1, 3, 1, 1,
    1, 2, 1
]

# Calcular porcentajes
total = sum(valores)
porcentajes = [(v / total) * 100 for v in valores]

# Crear gráfico de barras
plt.figure(figsize=(12, 6))
bars = plt.bar(familias, porcentajes, color='mediumseagreen')

# Agregar porcentajes encima de las barras
for bar, pct in zip(bars, porcentajes):
    plt.text(bar.get_x() + bar.get_width() / 2, bar.get_height() + 0.3,
             f"{pct:.1f}%", ha='center', va='bottom', fontsize=9)

# Personalización del gráfico
plt.title("Porcentaje de especies por familia", fontsize=14, fontweight='bold')
plt.xlabel("Familia", fontsize=12)
plt.ylabel("Porcentaje (%)", fontsize=12)
plt.xticks(rotation=75, ha='right')
plt.grid(axis='y', linestyle='--', alpha=0.6)
plt.tight_layout()

plt.show()
