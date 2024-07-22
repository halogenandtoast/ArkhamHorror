module Arkham.Investigator.Cards.SilasMarsh (
  silasMarsh,
  SilasMarsh (..),
)
where

import Arkham.Card
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner hiding (RevealChaosToken)
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype SilasMarsh = SilasMarsh InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

silasMarsh :: InvestigatorCard SilasMarsh
silasMarsh =
  investigator
    SilasMarsh
    Cards.silasMarsh
    Stats
      { health = 9
      , sanity = 5
      , willpower = 2
      , intellect = 2
      , combat = 4
      , agility = 4
      }

instance HasAbilities SilasMarsh where
  getAbilities (SilasMarsh attrs) =
    [ playerLimit PerRound
        $ restrictedAbility
          attrs
          1
          (Self <> DuringSkillTest (YourSkillTest AnySkillTest) <> exists (SkillControlledBy You))
        $ freeReaction
        $ RevealChaosToken #after You AnyChaosToken
    ]

instance HasChaosTokenValue SilasMarsh where
  getChaosTokenValue iid ElderSign (SilasMarsh attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 0)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage SilasMarsh where
  runMessage msg i@(SilasMarsh attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skills <- select (skillControlledBy iid)
      player <- getPlayer iid
      push
        $ chooseOrRunOne player [targetLabel skill [ReturnToHand iid (toTarget skill)] | skill <- skills]
      pure i
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      skills <-
        filterM (getIsCommittable iid)
          . filter (`cardMatch` CardWithType SkillType)
          =<< fieldMap InvestigatorDiscard (map toCard) iid

      when (notNull skills) do
        player <- getPlayer iid
        withSkillTest \sid -> do
          pushAll
            [ FocusCards skills
            , chooseOne player $ Label "Do not commit skills" []
                : [ targetLabel
                    (CardIdTarget $ toCardId card)
                    [ CommitCard iid card
                    , skillTestModifier @Source sid #elderSign (CardIdTarget $ toCardId card) ReturnToHandAfterTest
                    ]
                  | card <- skills
                  ]
            ]
      pure i
    _ -> SilasMarsh <$> runMessage msg attrs
