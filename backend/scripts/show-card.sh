if command -v kitten &> /dev/null
then
  kitten icat ../frontend/public/img/arkham/cards/${1}.jpg
elif command -v imgcat &> /dev/null
then
    # Execute kitten icat
  imgcat ../frontend/public/img/arkham/cards/${1}.jpg
else
    echo "Neither imgcat nor kitten icat is installed on this system."
    exit 1
fi
