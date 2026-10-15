import { tool } from "@opencode-ai/plugin";

const endpoint = process.env.WEB_SEARCH_ROUTER_URL || "http://127.0.0.1:8979";

export default tool({
	description:
		"Search current public web through the local multi-provider search router. Automatically tries available quality-ranked sources, with keyless fallback. Returns URLs, snippets, the source engine and errors; never fabricate results.",
	args: {
		query: tool.schema.string().min(2).max(256).describe("Web search query"),
	},
	async execute({ query }) {
		const abort = new AbortController();
		const timer = setTimeout(() => abort.abort(), 30000);
		try {
			const url = new URL("/search", endpoint);
			url.searchParams.set("q", query);
			url.searchParams.set("format", "json");
			const response = await fetch(url, {
				signal: abort.signal,
				headers: { Accept: "application/json" },
			});
			const data = await response.json();
			if (!response.ok)
				throw new Error(
					`Search router HTTP ${response.status}: ${data.error || "unavailable"}`,
				);
			if (!Array.isArray(data.results) || data.results.length === 0)
				throw new Error("No web search results");
			return JSON.stringify(data);
		} catch (error) {
			return `Web search unavailable: ${error instanceof Error ? error.message : String(error)}. Do not invent sources.`;
		} finally {
			clearTimeout(timer);
		}
	},
});
