#!/bin/bash
# ============================================================================
# Script de instalación de dependencias de SISTEMA para FAO_COLOMBIA en AWS
# ============================================================================
#
# Uso: sudo bash install_system_dependencies.sh
#
# Este script instala todas las dependencias necesarias a nivel de sistema
# operativo antes de instalar los paquetes R.
#
# Compatible con: Amazon Linux 2, CentOS 7+, RHEL 7+

set -e  # Salir si hay cualquier error

# Colores
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Funciones de utilidad
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[✓]${NC} $1"
}

log_error() {
    echo -e "${RED}[✗]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[!]${NC} $1"
}

# Verificar si se ejecuta como root
if [[ $EUID -ne 0 ]]; then
   log_error "Este script debe ejecutarse como root (use sudo)"
   exit 1
fi

# Inicio
echo ""
echo "╔════════════════════════════════════════════════════════════════╗"
echo "║  Instalador de dependencias de SISTEMA para FAO_COLOMBIA      ║"
echo "║  AWS - RServer - Diciembre 2025                              ║"
echo "╚════════════════════════════════════════════════════════════════╝"
echo ""

# Detectar distribución
if [ -f /etc/os-release ]; then
    . /etc/os-release
    OS=$NAME
    VERSION=$VERSION_ID
else
    log_error "No se puede detectar la distribución de Linux"
    exit 1
fi

log_info "Sistema detectado: $OS $VERSION"
log_info "Iniciando instalación de dependencias..."
echo ""

# Actualizar repositorios
log_info "Actualizando repositorios de paquetes..."
yum update -y > /dev/null 2>&1
log_success "Repositorios actualizados"

# Herramientas de compilación
log_info "Instalando herramientas de compilación..."
yum install -y \
    gcc \
    gcc-c++ \
    make \
    gfortran \
    pkgconfig \
    > /dev/null 2>&1
log_success "Herramientas de compilación instaladas"

# Dependencias para librerías de desarrollo
log_info "Instalando librerías de desarrollo..."
yum install -y \
    openssl-devel \
    libcurl-devel \
    libjpeg-turbo-devel \
    libpng-devel \
    cairo-devel \
    freetype-devel \
    harfbuzz-devel \
    fribidi-devel \
    zlib-devel \
    bzip2-devel \
    xz-devel \
    > /dev/null 2>&1
log_success "Librerías de desarrollo instaladas"

# Dependencias para datos geoespaciales (CRITICAL para sf)
log_info "Instalando dependencias geoespaciales (GDAL, GEOS, PROJ)..."
yum install -y \
    gdal-devel \
    geos-devel \
    proj-devel \
    sqlite-devel \
    > /dev/null 2>&1
log_success "Dependencias geoespaciales instaladas"

# Dependencias para webshot2 (captura de pantallas)
log_info "Instalando dependencias para captura de pantallas..."
yum install -y \
    chromium \
    xvfb \
    > /dev/null 2>&1
log_success "Dependencias para captura instaladas"

# Herramientas adicionales
log_info "Instalando herramientas adicionales..."
yum install -y \
    git \
    wget \
    curl \
    tree \
    htop \
    > /dev/null 2>&1
log_success "Herramientas adicionales instaladas"

# Verificación final
echo ""
log_info "Verificando instalaciones..."
echo ""

# Verificar herramientas
tools=("gcc" "g++" "gfortran" "make" "git")
for tool in "${tools[@]}"; do
    if command -v $tool &> /dev/null; then
        version=$($tool --version 2>/dev/null | head -n1)
        log_success "$tool: $version"
    else
        log_error "$tool no se encontró"
    fi
done

# Verificar librerías geoespaciales
log_info "Verificando librerías de datos geoespaciales..."
libs_geoespaciales=("gdal-config" "geos-config" "proj")
for lib in "${libs_geoespaciales[@]}"; do
    if command -v $lib &> /dev/null || [ -f "/usr/bin/$lib" ] || [ -f "/usr/local/bin/$lib" ]; then
        log_success "$lib encontrado"
    else
        log_warn "$lib no se encontró en la ruta estándar"
    fi
done

echo ""
echo "╔════════════════════════════════════════════════════════════════╗"
echo -e "${GREEN}✓ Instalación de dependencias completada${NC}"
echo "╚════════════════════════════════════════════════════════════════╝"
echo ""
log_info "Próximos pasos:"
echo "  1. Instalar RServer:"
echo "     wget https://rstudio.org/download/latest/daily/server/rhel/rstudio-server-latest.x86_64.rpm"
echo "     sudo yum install -y ./rstudio-server-latest.x86_64.rpm"
echo ""
echo "  2. Instalar paquetes R:"
echo "     Rscript install_dependencies.R"
echo ""
echo "  3. Clonar repositorio:"
echo "     git clone https://github.com/dcardoso-lopez/FAO_COLOMBIA.git"
echo ""
