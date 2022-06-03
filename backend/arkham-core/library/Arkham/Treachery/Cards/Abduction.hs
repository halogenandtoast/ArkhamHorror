module Arkham.Treachery.Cards.Abduction
  ( abduction
  , Abduction(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype Abduction = Abduction TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abduction :: TreacheryCard Abduction
abduction = treachery Abduction Cards.abduction

instance TreacheryRunner env => RunMessage Abduction where
  runMessage msg t@(Abduction attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (RevelationSkillTest iid source SkillWillpower 3)
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        allies <- map AssetTarget <$> selectList
          (AssetControlledBy You <> AllyAsset <> DiscardableAsset)
        case allies of
          [] -> t <$ push (LoseAllResources iid)
          targets -> t <$ push
            (chooseOne
              iid
              [ Label "Lose all resources" [LoseAllResources iid]
              , Label
                "Discard an Ally asset you control"
                [ chooseOne
                    iid
                    [ TargetLabel target [Discard target] | target <- targets ]
                ]
              ]
            )
    _ -> Abduction <$> runMessage msg attrs
