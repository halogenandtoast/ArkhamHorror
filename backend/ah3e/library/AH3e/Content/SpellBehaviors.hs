-- | Mechanics for spells whose text the effect vocabulary cannot express.
module AH3e.Content.SpellBehaviors (behaviors) where

import AH3e.Engine.Behavior
import AH3e.Engine.Helpers
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Skill
import AH3e.Types.State
import Data.Map.Strict qualified as Map

behaviors :: Behaviors
behaviors =
  mempty
    & #assets
    .~ Map.fromList
      [
        ( "alchemical-process"
        , spellAction "Alchemical Process: gain $1 for each success" 0 (GainE (Money TestResult))
        )
      , ("astral-travel", defaultAssetBehavior & #moveBySpell ?~ (Lore, 0, 2))
      , ("binding", binding)
      ,
        ( "find-gate"
        , spellAction "Find Gate: move to a space with doom" 0 (MoveDirectlyTo AnySpaceWithDoom)
        )
      , ("flesh-ward", fleshWard)
      , ("mists-of-rlyeh", defaultAssetBehavior & #evadeSkillInstead ?~ Lore)
      ,
        ( "healing-words"
        , spellAction
            "Healing Words: recover health"
            (-1)
            (RecoverHealth InvestigatorOrAllyInYourSpace TestResult)
        )
      ,
        ( "shriveling"
        , whileEngaged
            (spellAction "Shriveling: damage a monster" (-1) (DamageMonsterIn YourSpaceOrAdjacent TestResult))
        )
      ]

{- | The monster is chosen before the test, because the test uses that monster's
own evade modifier. Exhausting it drops its engagement with it, since a monster's
state holds either.
-}
binding :: AssetBehavior
binding =
  defaultAssetBehavior
    & #componentActions
    .~ [ ComponentActionDef
           { label = "Binding: exhaust a monster"
           , allowedWhileEngaged = False
           , canPerform = \_ -> not . null <$> uses #monsters Map.elems
           , perform = \ctx -> case ctx.source of
               SourceCard cid -> do
                 ms <- uses #monsters Map.elems
                 choices <- for ms \m -> do
                   d <- monsterDef m.card
                   pure
                     (Choice (MonsterLabel m.card) [castingTest ctx cid d.evadeModifier (AfterExhaustMonster m.card)])
                 chooseFor ctx.investigator "Choose a monster to bind" choices
               _ -> pure ()
           }
       ]

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
