const fs = require('fs');

['index.html', 'styles.css'].forEach(file => fs.copyFileSync(`public/${file}`, `dist/${file}`));
