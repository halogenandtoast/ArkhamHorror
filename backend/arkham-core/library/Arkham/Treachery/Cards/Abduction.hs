module Arkham.Treachery.Cards.Abduction (
  abduction,
  Abduction (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Abduction = Abduction TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

abduction :: TreacheryCard Abduction
abduction = treachery Abduction Cards.abduction

instance RunMessage Abduction where
  runMessage msg t@(Abduction attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ revelationSkillTest iid attrs #willpower 3
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      allies <- selectTargets $ assetControlledBy iid <> #ally <> DiscardableAsset
      case allies of
        [] -> push $ LoseAllResources iid
        targets -> do
          player <- getPlayer iid
          push
            $ chooseOne player
            $ [ Label "Lose all resources" [LoseAllResources iid]
              , Label
                  "Discard an Ally asset you control"
                  [chooseOne player $ targetLabels targets (only . toDiscardBy iid attrs)]
              ]
      pure t
    _ -> Abduction <$> runMessage msg attrs
