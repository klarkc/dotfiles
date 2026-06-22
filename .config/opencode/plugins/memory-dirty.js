import { readFile, stat, writeFile } from "node:fs/promises"
import { join } from "node:path"

const DEFAULT_THRESHOLD_BYTES = 8 * 1024

const MEMORIES = [
  ".agents/build/MEMORY.md",
  ".agents/design/MEMORY.md",
]

const MANAGED_KEYS = new Set([
  "memory_version",
  "status",
  "dirty_reason",
  "size_bytes",
  "threshold_bytes",
  "checked_at",
])

function thresholdBytes() {
  const raw = Number(process.env.OPENCODE_MEMORY_DIRTY_THRESHOLD_BYTES)
  return Number.isFinite(raw) && raw > 0 ? raw : DEFAULT_THRESHOLD_BYTES
}

async function fileSize(path) {
  try {
    return (await stat(path)).size
  } catch {
    return 0
  }
}

function splitFrontmatter(content) {
  if (!content.startsWith("---\n")) return { frontmatter: "", body: content }

  const end = content.indexOf("\n---", 4)
  if (end === -1) return { frontmatter: "", body: content }

  let body = content.slice(end + "\n---".length)
  if (body.startsWith("\n")) body = body.slice(1)

  return {
    frontmatter: content.slice(4, end),
    body,
  }
}

function hasDirtyStatus(frontmatter) {
  return /^status:\s*dirty\s*$/m.test(frontmatter)
}

function markDirty(content, size, threshold) {
  const { frontmatter, body } = splitFrontmatter(content)

  // Avoid repeated writes/noisy diffs while the agent has not compacted yet.
  if (hasDirtyStatus(frontmatter)) return null

  const preserved = frontmatter
    .split("\n")
    .filter((line) => {
      const key = line.split(":", 1)[0]?.trim()
      return key && !MANAGED_KEYS.has(key)
    })

  const managed = [
    "memory_version: 1",
    "status: dirty",
    "dirty_reason: size-threshold-exceeded",
    `size_bytes: ${size}`,
    `threshold_bytes: ${threshold}`,
    `checked_at: ${new Date().toISOString()}`,
  ]

  const front = [...managed, ...preserved].join("\n")
  return `---\n${front}\n---\n\n${body.trimStart()}`
}

export const MemoryDirtyPlugin = async ({ worktree }) => {
  return {
    event: async ({ event }) => {
      if (event.type !== "session.compacted") return

      const threshold = thresholdBytes()

      for (const memory of MEMORIES) {
        const memoryPath = join(worktree, memory)
        const size = await fileSize(memoryPath)
        if (size <= threshold) continue

        const content = await readFile(memoryPath, "utf8")
        const next = markDirty(content, size, threshold)
        if (next === null) continue

        await writeFile(memoryPath, next)
      }
    },
  }
}
