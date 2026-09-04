# Online supplement figures

These exploratory figures are retained for the online supplement but omitted
from the ten-page review manuscript:

- `abs_challenge_locations_by_role.png`: observed challenge locations by role.
- `abs_challenge_rate_by_inning.png`: the redundant observed-only inning view.

All manuscript and supplement exhibits use the LTC Reading palette and are built
by `paper/build_figures.R`. The inning figure is regenerated from the versioned
`results/` bundle. Regenerating the location figure additionally requires the
processed challenge-events file; the styled output itself is retained here.
