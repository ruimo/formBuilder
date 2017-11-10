package com.ruimo.forms

class RestException(val statusCode: Int, val statusText: String, val body: String)
    extends RuntimeException(statusCode + " '" + statusText + "': " + body)
