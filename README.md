# Hndrx

Work in progress.

## Environment boot-up

    python -m http.server
    docker run -it --rm -p 9000:9000 dagrevis/peerserver
    lein cljsbuild auto

Go to http://127.0.0.1:8000/ with modern browser.

## Integration tests

    git clone https://github.com/daGrevis/hndrx-tests
    lein test
