-- | Mechanics for spells whose text the effect vocabulary cannot express.
module AH3e.Content.SpellBehaviors (behaviors) where

import AH3e.Engine.Behavior
import AH3e.Engine.Helpers
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Skill
import AH3e.Types.State
import Data.Map.Strict qualified as Map

behaviors :: Behaviors
behaviors = mempty & #assets .~ Map.fromList [("flesh-ward", fleshWard)]

{- | Once per round, cast to prevent damage equal to a lore test result. The
damage may be anyone's, so 'damagePrevention' offers it wherever the sufferer
is; marking the card used before the cast keeps the cast's own damage, which
Agnes may pay, from offering it again.
-}
fleshWard :: AssetBehavior
fleshWard =
  defaultAssetBehavior
    & #damagePrevention
    .~ \cid owner plan ->
      pure
        [ Reaction
            ("flesh-ward-" <> tshow cid)
            "Flesh Ward: test lore to prevent damage"
            [ MarkAssetUsed owner cid
            , CastSpell
                owner
                cid
                [BeginTest (newTest owner Lore 0 (SpellTest cid) AfterPreventDamage) {casting = Just cid}]
            ]
        | plan.damage > 0
        ]
