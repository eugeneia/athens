(in-package :percent.test)

(5am:in-suite* all)

(5am:test encode
  (5am:is (equal "Aa0-._~"
                 (percent:encode "Aa0-._~")))
  (5am:is (equal "%3A%2F%3F%23%5B%5D%40%21%24%26%27%28%29%2A%2B%2C%3B%3D"
                 (percent:encode ":/?#[]@!$&'()*+,;="))))

(5am:test encode/test
  (5am:is (equal "Aa0-._~"
                 (percent:encode "Aa0-._~"
                                 :test (constantly t))))
  (5am:is (equal ":/?#[]@!$&'()*+,;="
                 (percent:encode ":/?#[]@!$&'()*+,;="
                                 :test (constantly t))))
  (5am:is (equal "%41%61%30%2D%2E%5F%7E"
                 (percent:encode "Aa0-._~"
                                 :test (constantly nil))))
  (5am:is (equal "%3A%2F%3F%23%5B%5D%40%21%24%26%27%28%29%2A%2B%2C%3B%3D"
                 (percent:encode ":/?#[]@!$&'()*+,;="
                                 :test (constantly nil))))
  (5am:is (equal ":%2F%3F%23%5B%5D@!$&'()*+,;="
                 (percent:encode ":/?#[]@!$&'()*+,;="
                                 :test #'pcharp))))

(5am:test encode/encoding
  (5am:is (equal "Aa0-._~"
                 (percent:encode "Aa0-._~"
                                 :encoding :ascii)))
  (5am:is (equal "%3A%2F%3F%23%5B%5D%40%21%24%26%27%28%29%2A%2B%2C%3B%3D"
                 (percent:encode ":/?#[]@!$&'()*+,;="
                                 :encoding :ascii)))
  (5am:is (equal "Aa0-._~"
                 (percent:encode "Aa0-._~"
                                 :encoding :utf-8)))
  (5am:is (equal "%3A%2F%3F%23%5B%5D%40%21%24%26%27%28%29%2A%2B%2C%3B%3D"
                 (percent:encode ":/?#[]@!$&'()*+,;="
                                 :encoding :utf-8)))
  (5am:is (equal "%E6%97%A5%E6%9C%AC%E8%AA%9E"
                 (percent:encode "日本語"
                                 :encoding :utf-8)))
  (5am:is (equal "%93%FA%96%7B%8C%EA"
                 (percent:encode "日本語"
                                 :encoding :cp932))))

(5am:test decode
  (5am:is (equal "Aa0-._~"
                 (percent:decode "Aa0-._~")))
  (5am:is (equal ":/?#[]@!$&'()*+,;="
                 (percent:decode "%3A%2F%3F%23%5B%5D%40%21%24%26%27%28%29%2A%2B%2C%3B%3D"))))

(5am:test decode/test
  (5am:is (equal "Aa0-._~"
                 (percent:decode "Aa0-._~"
                                 :test (constantly nil))))
  (5am:is (equal ":/?#[]@!$&'()*+,;="
                 (percent:decode ":/?#[]@!$&'()*+,;="
                                 :test (constantly nil))))
  (5am:is (equal "Aa0-._~"
                 (percent:decode "%41%61%30%2D%2E%5F%7E"
                                 :test (constantly t))))
  (5am:is (equal ":/?#[]@!$&'()*+,;="
                 (percent:decode "%3A%2F%3F%23%5B%5D%40%21%24%26%27%28%29%2A%2B%2C%3B%3D"
                                 :test (constantly t))))
  (5am:is (equal ":/?#[]@!$&'()*+,;="
                 (percent:decode ":%2F%3F%23%5B%5D@!$&'()*+,;="
                                 :test (complement #'pcharp)))))

(5am:test decode/encoding
  (5am:is (equal "Aa0-._~"
                 (percent:decode "Aa0-._~"
                                 :encoding :ascii)))
  (5am:is (equal ":/?#[]@!$&'()*+,;="
                 (percent:decode "%3A%2F%3F%23%5B%5D%40%21%24%26%27%28%29%2A%2B%2C%3B%3D"
                                 :encoding :ascii)))
  (5am:is (equal "Aa0-._~"
                 (percent:decode "Aa0-._~"
                                 :encoding :utf-8)))
  (5am:is (equal ":/?#[]@!$&'()*+,;="
                 (percent:decode "%3A%2F%3F%23%5B%5D%40%21%24%26%27%28%29%2A%2B%2C%3B%3D"
                                 :encoding :utf-8)))
  (5am:is (equal "日本語"
                 (percent:decode "%E6%97%A5%E6%9C%AC%E8%AA%9E"
                                 :encoding :utf-8)))
  (5am:is (equal "日本語"
                 (percent:decode "%93%FA%96%7B%8C%EA"
                                 :encoding :cp932))))
