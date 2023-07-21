module Arkham.Treachery.Cards.SnakeBite (
  snakeBite,
  SnakeBite (..),
) where

import Arkham.Prelude

import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype SnakeBite = SnakeBite TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

snakeBite :: TreacheryCard SnakeBite
snakeBite = treachery SnakeBite Cards.snakeBite

instance RunMessage SnakeBite where
  runMessage msg t@(SnakeBite attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      push $ RevelationSkillTest iid source SkillAgility 3
      pure t
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ _ -> do
      allies <-
        selectList $
          AllyAsset
            <> AssetControlledBy
              (InvestigatorWithId iid)
      isPoisoned <- getIsPoisoned iid
      handlePoisoned <-
        if isPoisoned
          then pure []
          else do
            poisoned <- getSetAsidePoisoned
            pure [CreateWeaknessInThreatArea poisoned iid]
      push $
        chooseOrRunOne iid $
          [ Label
            "Deal 5 damage to an Ally asset you control"
            [chooseOne iid [targetLabel ally [AssetDamage ally source 5 0] | ally <- allies]]
          | notNull allies
          ]
            <> [ Label
                  "Take 1 direct damage. If you are not poisoned, put a set-aside Poisoned weakness into play in your threat area."
                  $ InvestigatorDirectDamage iid source 1 0
                    : handlePoisoned
               ]
      pure t
    _ -> SnakeBite <$> runMessage msg attrs
