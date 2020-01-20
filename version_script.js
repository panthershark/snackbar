const { writeFileSync } = require('fs');

const packageJSON = require('./package.json');
const { version } = require('./elm.json');

packageJSON.version = version;
writeFileSync('package.json', JSON.stringify(packageJSON, null, 2));
