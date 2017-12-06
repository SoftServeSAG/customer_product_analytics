# OCEAN recognition demo for AWS

This repository contains code from the demom for OCEAN recognition based on textual data and buying patterns

## Docker install ##

1. Get application source code:
```bash
git clone https://github.com/SoftServeSAG/customer_product_analytics
cd customer_product_analytics/
```

2. Get application data:
```bash
wget https://s3.amazonaws.com/softserve-analytics/customer_product_analytics.zip
unzip customer_product_analytics.zip
```

2. Build docker image:
```bash
docker build --rm --force-rm -t customer_product_analytics .
```

3. Run docker image:
```bash
docker run --rm -p 3838:3838 customer_product_analytics
```

4. Open your favorite browser and navigate to the `http://127.0.0.1:3838/customer_product_analytics/`