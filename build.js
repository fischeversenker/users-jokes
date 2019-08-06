const fs = require('fs');

const publicFiles = fs.readdirSync('public');
publicFiles.forEach(file => fs.copyFileSync(`public/${file}`, `dist/${file}`));
