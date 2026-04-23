# Vast.ai Deployment Baseline

## Configuration Defaults

### Model Settings
| Setting | Value |
|---------|-------|
| Model | Qwen/Qwen3.6-27B-FP8 |
| Max Context | 49152 tokens |
| VLLM Image | vllm/vllm-openai:v0.9.1 |

### Pricing Constraints
| Parameter | Value |
|-----------|-------|
| Max Price | $0.35/hour |
| Bid Price | $0.33/hour |
| Price Margin | ~5.7% buffer |

### Quality Constraints
| Parameter | Value |
|-----------|-------|
| Min Reliability | 0.985 |
| Min Disk | 100 GB |

### GPU Preference Order
1. L40S (primary)
2. RTX_5090 (secondary)
3. A100_PCIE (tertiary)
4. RTX_4090 (fallback)

## Assumptions

### 1. GPU Performance Hierarchy
- L40S expected to have highest DLPerf for 27B models
- RTX 5090 is current-gen consumer flagship with strong FP8 support
- A100_PCIE offers enterprise reliability but may be overpriced
- RTX 4090 is previous-gen, included as fallback only

### 2. Reliability Threshold
- 0.985 minimum ensures <1.5% failure rate
- Based on historical uptime data from vast.ai
- Higher reliability typically correlates with better pricing stability

### 3. Price Sensitivity
- $0.33 bid allows for ~6% market fluctuation
- $0.35 hard cap prevents runaway costs
- Target: secure GPU at $0.28-0.31 for optimal value

### 4. Market Dynamics
- RTX 5090 currently dominates available inventory
- L40S may be scarce or command premium pricing
- Geographic diversity in top candidates (Thailand, China, US, Vietnam, Korea)

## Known Constraints

### 1. Search Limitation
- Only searches 5 GPU types (L40S, RTX_5090, A100_PCIE, RTX_4090)
- May miss better value GPUs (e.g., RTX 6000, A6000)
- Limited to top 5 results by default

### 2. Reliability vs. Availability Trade-off
- 0.985 threshold may exclude cheaper but viable options
- Some 0.97-0.98 offers may provide better value
- Consider lowering threshold if no offers found

### 3. Single GPU Selection
- Script selects only one GPU per launch
- No load balancing or redundancy
- Instance failure requires manual restart

## Operational Notes

### Dry-Run Mode
- Use `DRY_RUN=1` or `--dry-run` for non-interactive validation
- Shows candidate selection without creating instance
- Useful for market monitoring without commitment

### Environment Variables Required
- `VAST_API_KEY`: Vast.ai API authentication
- `HF_TOKEN`: HuggingFace model download access

### Cancellation Behavior
- Pressing 'n' at prompt exits cleanly with code 0
- No instance created, no charges incurred
- Safe for repeated testing

## Future Improvements

1. **Expand GPU Search**: Add RTX_6000, A6000, H100 options
2. **Dynamic Pricing**: Adjust bid based on time of day/availability
3. **Multi-GPU Selection**: Rank top 3 and create instances if primary fails
4. **Historical Tracking**: Log all bids and outcomes for analysis
5. **Alert System**: Notify when L40S becomes available under $0.30

---
