module Arkham.Types.Asset.Cards.TryAndTryAgain3
  ( tryAndTryAgain3
  , TryAndTryAgain3(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window

newtype TryAndTryAgain3 = TryAndTryAgain3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tryAndTryAgain3 :: AssetCard TryAndTryAgain3
tryAndTryAgain3 = asset TryAndTryAgain3 Cards.tryAndTryAgain3

ability :: AssetAttrs -> Ability
ability a = mkAbility a 1 (ResponseAbility $ ExhaustCost (toTarget a))

instance HasList CommittedCard env InvestigatorId => HasAbilities env TryAndTryAgain3 where
  getAbilities iid (AfterFailSkillTest _ _) (TryAndTryAgain3 attrs) = do
    committedSkills <-
      filter ((== SkillType) . toCardType) . map unCommittedCard <$> getList iid
    pure [ ability attrs | notNull committedSkills ]
  getAbilities iid window (TryAndTryAgain3 attrs) = getAbilities iid window attrs

instance HasModifiersFor env TryAndTryAgain3

instance
  ( HasList CommittedCard env InvestigatorId
  , HasQueue env, HasModifiersFor env ()
  )
  => RunMessage env TryAndTryAgain3 where
  runMessage msg a@(TryAndTryAgain3 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      committedSkills <-
        filter ((== SkillType) . toCardType)
        . map unCommittedCard
        <$> getList iid
      a <$ pushAll
        [ FocusCards committedSkills
        , chooseOne
          iid
          [ ReturnToHand iid (SkillTarget $ SkillId $ toCardId skill)
          | skill <- committedSkills
          ]
        , UnfocusCards
        ]
    _ -> TryAndTryAgain3 <$> runMessage msg attrs
