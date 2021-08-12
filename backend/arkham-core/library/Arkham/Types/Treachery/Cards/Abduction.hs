module Arkham.Types.Treachery.Cards.Abduction
  ( abduction
  , Abduction(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype Abduction = Abduction TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abduction :: TreacheryCard Abduction
abduction = treachery Abduction Cards.abduction

instance TreacheryRunner env => RunMessage env Abduction where
  runMessage msg t@(Abduction attrs) = case msg of
    Revelation iid source | isSource attrs source -> t <$ pushAll
      [ RevelationSkillTest iid source SkillWillpower 3
      , Discard $ toTarget attrs
      ]
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        allies <- map AssetTarget <$> selectList
          (AssetOwnedBy You <> AssetWithTrait Ally <> DiscardableAsset)
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
