(define-generic-mode 'htaccess-mode
  '(?#)
  '(;; core
    "AcceptPathInfo" "AccessFileName" "AddDefaultCharset" "AddOutputFilterByType"
    "AllowEncodedSlashes" "AllowOverride" "AuthName" "AuthType"
    "CGIMapExtension" "ContentDigest" "DefaultType" "DocumentRoot"
    "EnableMMAP" "EnableSendfile" "ErrorDocument" "ErrorLog"
    "FileETag" "ForceType" "HostnameLookups" "IdentityCheck"
    "Include" "KeepAlive" "KeepAliveTimeout" "LimitInternalRecursion"
    "LimitRequestBody" "LimitRequestFields" "LimitRequestFieldSize" "LimitRequestLine"
    "LimitXMLRequestBody" "LogLevel" "MaxKeepAliveRequests" "NameVirtualHost"
    "Options" "Require" "RLimitCPU" "RLimitMEM"
    "RLimitNPROC" "Satisfy" "ScriptInterpreterSource" "ServerAdmin"
    "ServerAlias" "ServerName" "ServerPath" "ServerRoot"
    "ServerSignature" "ServerTokens" "SetHandler" "SetInputFilter"
    "SetOutputFilter" "TimeOut" "UseCanonicalName"
    ;; .htaccess tutorial
    "AddHandler" "AuthUserFile" "AuthGroupFile"
    ;; mod_rewrite
    "RewriteBase" "RewriteCond" "RewriteEngine" "RewriteLock" "RewriteLog"
    "RewriteLogLevel" "RewriteMap" "RewriteOptions" "RewriteRule"
    ;; mod_alias
    "Alias" "AliasMatch" "Redirect" "RedirectMatch" "RedirectPermanent"
    "RedirectTemp" "ScriptAlias" "ScriptAliasMatch")
  '(("%{\\([A-Z_]+\\)}" 1 font-lock-variable-name-face)
    ("\\b[0-9][0-9][0-9]\\b" . font-lock-constant-face)
    ("\\[.*\\]" . font-lock-type-face))
  '(".htaccess\\'")
  nil
  "Generic mode for Apache .htaccess files.")
