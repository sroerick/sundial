(job "backup-agent-memory"
  (schedule "0 9 * * 1")
  (tz "America/Denver")
  (cmd "/home/roerick/wyo/saguaro/ops/backup-conversations.sh")
  (timeout 1800)
  (overlap skip)
  (on-miss once)
  (heartbeat 691200)
  (tags ("backup" "agent-memory")))
