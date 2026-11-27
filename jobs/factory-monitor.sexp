(job "factory-monitor"
  (schedule "*/15 * * * *")
  (tz "UTC")
  (cmd "/bin/ksh" "/home/al/factory-monitor/monitor.sh")
  (timeout 300)
  (overlap skip)
  (on-miss skip)
  (heartbeat 3600)
  (tags ("factory" "monitor")))
