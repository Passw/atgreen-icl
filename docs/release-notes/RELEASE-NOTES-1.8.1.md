# ICL 1.8.1 Release Notes

## Security Improvements

### MCP Server Hardening
- **Capability URL**: MCP endpoint now includes a random 32-character token in the URL path, preventing other local processes from connecting without knowing the URL
- **Single session lock**: Only one client can initialize an MCP session; additional attempts are rejected
- **Auto-shutdown**: MCP server automatically stops after 5 minutes of inactivity

## Breaking Changes

None.
