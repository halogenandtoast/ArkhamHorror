export const handleI18n = (body: string, t: (key: string, params: { [key: string]: any }) => string) => {
  const {key, params} = parseInput(body)
  return t(key, params)
}

function parseInput(input: string) {
    // Remove leading and trailing whitespace
    input = input.trim();

    // Check for optional '$' at the beginning
    if (input.startsWith('$')) {
        input = input.substring(1).trim();
    }

    // Extract the key
    const spaceIndex = input.indexOf(' ');
    let key;
    let paramsString;

    if (spaceIndex === -1) {
        // No parameters present
        key = input;
        paramsString = '';
    } else {
        key = input.substring(0, spaceIndex);
        paramsString = input.substring(spaceIndex + 1).trim();
    }

    // Parse parameters
    const params = {} as { [key: string]: any };
    if (paramsString) {
        const tokens = tokenizeParams(paramsString);

        tokens.forEach(token => {
            // Match the pattern: name=type:value
            const match = token.match(/^([^=]+)=([is]):(.+)$/);
            if (!match) {
                throw new Error(`Invalid parameter format: ${token}`);
            }

            const paramName = match[1];
            const paramType = match[2];
            let paramValue = match[3];

            // Parse the parameter value based on its type
            if (paramType === 'i') {
                const intValue = parseInt(paramValue, 10);
                if (isNaN(intValue)) {
                    throw new Error(`Invalid integer value for parameter "${paramName}"`);
                }
                params[paramName] = intValue;
            } else if (paramType === 's') {
                // Handle quoted strings
                if ((paramValue.startsWith('"') && paramValue.endsWith('"')) ||
                    (paramValue.startsWith("'") && paramValue.endsWith("'"))) {
                    paramValue = paramValue.substring(1, paramValue.length - 1);
                } else {
                    throw new Error(`String parameter "${paramName}" must be enclosed in quotes`);
                }
                params[paramName] = paramValue;
            } else {
                throw new Error(`Unknown parameter type "${paramType}" for parameter "${paramName}"`);
            }
        });
    }

    return { key, params };
}

function tokenizeParams(paramsString: string) {
    const tokens = [];
    let currentToken = '';
    let inQuotes = false;
    let quoteChar = '';

    for (let i = 0; i < paramsString.length; i++) {
        const c = paramsString[i];

        if (inQuotes) {
            currentToken += c;
            if (c === quoteChar) {
                inQuotes = false;
            }
        } else {
            if (c === ' ') {
                if (currentToken.length > 0) {
                    tokens.push(currentToken);
                    currentToken = '';
                }
            } else {
                currentToken += c;
                if (c === '"' || c === "'") {
                    inQuotes = true;
                    quoteChar = c;
                }
            }
        }
    }

    if (currentToken.length > 0) {
        tokens.push(currentToken);
    }

    return tokens;
}
