module Arkham.Asset.Cards.JimsTrumpet (JimsTrumpet (..), jimsTrumpet) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Placement
import Arkham.Prelude

newtype JimsTrumpet = JimsTrumpet AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

jimsTrumpet :: AssetCard JimsTrumpet
jimsTrumpet = asset JimsTrumpet Cards.jimsTrumpet

instance HasAbilities JimsTrumpet where
  getAbilities (JimsTrumpet x) =
    [ controlledAbility
        x
        1
        ( exists
            $ HealableInvestigator (toSource x) #horror
            $ oneOf [InvestigatorAt YourLocation, InvestigatorAt ConnectedLocation]
        )
        $ ReactionAbility (RevealChaosToken #when Anyone (ChaosTokenFaceIs Skull)) (exhaust x)
    ]

instance RunMessage JimsTrumpet where
  runMessage msg a@(JimsTrumpet attrs@AssetAttrs {..}) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      case assetPlacement of
        InPlayArea controller -> do
          location <- getJustLocation controller
          investigatorsWithHeal <-
            getInvestigatorsWithHealHorror attrs 1
              $ oneOf [colocatedWith controller, InvestigatorAt (accessibleFrom location)]

          player <- getPlayer controller
          push
            $ chooseOne
              player
              [ targetLabel iid [healHorror]
              | (iid, healHorror) <- investigatorsWithHeal
              ]
          pure a
        _ -> error "Invalid call"
    _ -> JimsTrumpet <$> runMessage msg attrs
