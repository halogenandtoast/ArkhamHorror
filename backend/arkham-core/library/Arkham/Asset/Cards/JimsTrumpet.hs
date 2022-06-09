module Arkham.Asset.Cards.JimsTrumpet
  ( JimsTrumpet(..)
  , jimsTrumpet
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Investigator.Attrs (Field(..))
import Arkham.Projection
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token

newtype JimsTrumpet = JimsTrumpet AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jimsTrumpet :: AssetCard JimsTrumpet
jimsTrumpet = asset JimsTrumpet Cards.jimsTrumpet

instance HasAbilities JimsTrumpet where
  getAbilities (JimsTrumpet x) =
    [ restrictedAbility
        x
        1
        (OwnsThis <> InvestigatorExists
          (AnyInvestigator
              [InvestigatorAt YourLocation, InvestigatorAt ConnectedLocation]
          <> InvestigatorWithAnyHorror
          )
        )
        (ReactionAbility
          (RevealChaosToken Timing.When Anyone (TokenFaceIs Skull))
          (ExhaustCost $ toTarget x)
        )
    ]

instance RunMessage JimsTrumpet where
  runMessage msg a@(JimsTrumpet attrs@AssetAttrs {..}) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      let controllerId = fromJustNote "must be controller" assetController
      locationId <- fieldF InvestigatorLocation (fromJustNote "must be at a location") controllerId
      connectedLocationIds <- selectList $ AccessibleFrom $ LocationWithId locationId
      investigatorIds <-
        concat <$> for (locationId : connectedLocationIds) (selectList . InvestigatorAt . LocationWithId)
      pairings <- for investigatorIds
        $ \targetId -> (targetId, ) <$> field InvestigatorHorror targetId
      let choices = map fst $ filter ((> 0) . snd) pairings
      a <$ push
        (chooseOne
          controllerId
          [ HealHorror (InvestigatorTarget iid) 1 | iid <- choices ]
        )
    _ -> JimsTrumpet <$> runMessage msg attrs
