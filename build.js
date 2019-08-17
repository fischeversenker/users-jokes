const fs = require('fs');

const publicFiles = fs.readdirSync('public');

// make sure dist directory exists
if (!fs.existsSync('dist')) {
  fs.mkdirSync('dist');
}

publicFiles.forEach(file => fs.copyFileSync(`public/${file}`, `dist/${file}`));
