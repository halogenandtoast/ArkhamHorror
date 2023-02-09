module Arkham.Treachery.Cards.Abduction
  ( abduction
  , Abduction(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Treachery.Runner
import Arkham.Treachery.Cards qualified as Cards

newtype Abduction = Abduction TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abduction :: TreacheryCard Abduction
abduction = treachery Abduction Cards.abduction

instance RunMessage Abduction where
  runMessage msg t@(Abduction attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (RevelationSkillTest iid source SkillWillpower 3)
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        allies <- selectListMap
          AssetTarget
          (AssetControlledBy You <> AllyAsset <> DiscardableAsset)
        case allies of
          [] -> push $ LoseAllResources iid
          targets -> push $ chooseOne
            iid
            [ Label "Lose all resources" [LoseAllResources iid]
            , Label
              "Discard an Ally asset you control"
              [ chooseOne
                  iid
                  [ TargetLabel target [Discard (toSource attrs) target] | target <- targets ]
              ]
            ]
        pure t
    _ -> Abduction <$> runMessage msg attrs
