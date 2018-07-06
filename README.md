# httpc bench
Erlang HTTP client benchmarking suite

## How-to
#### Starting  server:

```
./bin/server.sh
Server started...
```

#### Running benchmark:

```
./bin/run.sh
Running benchmark...
```

## Benchmarks

#### Methodology

Benchmarks were executed on two servers, one client and one server.

-  Intel(R) Core(TM) i7-8550U CPU @ 1.80GHz
- Latency between the two servers < 1 ms
- Erlang 20.2

#### Results

```
Client  PoolSize  Concurency  Requests/s  Error %
=================================================
hackney        5           4        6041      0.0
hackney        9           8       11177      0.0
hackney       17          16       19161      0.0
hackney       33          32       17584      0.0
hackney       65          64       19683      0.0
hackney      129         128       16425      0.0
hackney      257         256       14114      0.0
hackney      513         512       11627      0.0
hackney     1025        1024        8138      0.0
ibrowse        5           4        8446      0.0
ibrowse        9           8       15443      0.0
ibrowse       17          16       23220      0.0
ibrowse       33          32       20490      0.0
ibrowse       65          64       23644      0.0
ibrowse      129         128       23071      0.0
ibrowse      257         256       16976      0.0
ibrowse      513         512       10996      0.0
ibrowse     1025        1024        6699      0.0
```



## 补充

感谢原作者提供了测试工具，作者测试姿势不太对，在PoolSize不够的情况下，增加Concurency是不对的，导致抢到资源的就发送请求，抢不到就空等，导致reqs上不去。所以我的测试就是保证Concurency比PoolSize小，保证并发的时候可以拿到资源来进行请求。



上述的数据是基于原作者的server跑的，实现上也有点投机，连续收包之后再批量回包，这样可以增加吞吐量。实际生产环境使用的api都是稍微运算下，然后再进行回复。所以附上，我测试我们内部api的数据，reqs小了快一半。



```
Client  PoolSize  Concurency  Requests/s  Error %
=================================================
hackney        5           4        1954      0.0
hackney        9           8        3526      0.0
hackney       17          16        6608      0.0
hackney       33          32        8400      0.0
hackney       65          64        9804      0.0
hackney      129         128       10007      0.0
hackney      257         256        9079      0.0
hackney      513         512       10272      0.0
hackney     1025        1024        9089      0.0
ibrowse        5           4        2090      0.2
ibrowse        9           8        3667      3.9
ibrowse       17          16        5243      9.6
ibrowse       33          32        6463      8.5
ibrowse       65          64        7429      0.9
ibrowse      129         128        7668      0.1
ibrowse      257         256        7299      0.0
ibrowse      513         512        7007      0.0
ibrowse     1025        1024        7763      0.0

```



另外，我们留意到ibrowse有一些Error，原因未知。



## 备注

如果在Linux遇到hdr_histogram_erl无法编译的话，可以参考https://github.com/HdrHistogram/hdr_histogram_erl/issues/33，在Linux分支，添加` -D_POSIX_C_SOURCE=199309L `