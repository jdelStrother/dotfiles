#!/usr/bin/env node

var stats = require('docker-stats');
var through = require('through2');
var turtle = (require('turtle-race'))({keep: false});
stats({statsinterval: 1}).pipe(through.obj(function(container, enc, cb) {
  var s = container.stats
  var cpu = s.cpu_stats.cpu_usage.cpu_percent
  var mem = 100 * s.memory_stats.usage / s.memory_stats.limit
  if (process.argv[2] === "memory") {
    turtle.metric(container.name, "memory").push(mem);
  } else {
    turtle.metric(container.name, "cpu").push(cpu);
  }
  return cb();
}));
