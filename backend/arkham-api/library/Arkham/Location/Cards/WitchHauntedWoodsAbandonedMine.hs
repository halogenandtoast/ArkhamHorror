module Arkham.Location.Cards.WitchHauntedWoodsAbandonedMine (
  witchHauntedWoodsAbandonedMine,
  WitchHauntedWoodsAbandonedMine (..),
) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message (getChoiceAmount)
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype WitchHauntedWoodsAbandonedMine = WitchHauntedWoodsAbandonedMine LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntedWoodsAbandonedMine :: LocationCard WitchHauntedWoodsAbandonedMine
witchHauntedWoodsAbandonedMine =
  location
    WitchHauntedWoodsAbandonedMine
    Cards.witchHauntedWoodsAbandonedMine
    2
    (PerPlayer 1)

instance HasModifiersFor WitchHauntedWoodsAbandonedMine where
  getModifiersFor (WitchHauntedWoodsAbandonedMine a) = do
    modifySelect
      a
      (InvestigatorWithResources (atLeast 3) <> InvestigatorWithResources (atMost 10))
      [CannotInvestigateLocation a.id]

instance HasAbilities WitchHauntedWoodsAbandonedMine where
  getAbilities (WitchHauntedWoodsAbandonedMine a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted
        a
        1
        ( Here
            <> ( exists
                  $ InvestigatorWithAnyResources
                  <> oneOf [You, InvestigatorAt (not_ $ LocationWithInvestigator You)]
               )
        )
      $ FastAbility Free

-- Note: we above ProxyTarget here by doubling it to include the two and from,
-- it would be nice to have a better way to handle this
instance RunMessage WitchHauntedWoodsAbandonedMine where
  runMessage msg l@(WitchHauntedWoodsAbandonedMine attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resources <- field InvestigatorResources iid
      let checkResources = if resources > 0 then id else (<> InvestigatorWithAnyResources)
      iids <-
        selectWithField InvestigatorResources
          $ checkResources
          $ InvestigatorAt (not_ $ locationWithInvestigator iid)

      chooseOneM iid do
        for_ iids \(iid', otherResources) -> do
          targeting iid' do
            chooseOrRunOneM iid do
              when (resources > 0) do
                labeled "Move to their pool"
                  $ chooseAmounts
                    iid
                    "Choose amount of resources to move"
                    (MaxAmountTarget 3)
                    [("Resources", (0, resources))]
                  $ ProxyTarget (toTarget attrs)
                  $ ProxyTarget (InvestigatorTarget iid) (InvestigatorTarget iid')
              when (otherResources > 0) do
                labeled "Move to your pool"
                  $ chooseAmounts
                    iid
                    "Choose amount of resources to move"
                    (MaxAmountTarget 3)
                    [("Resources", (0, otherResources))]
                  $ ProxyTarget (toTarget attrs)
                  $ ProxyTarget (InvestigatorTarget iid') (InvestigatorTarget iid)

      pure l
    ResolveAmounts
      _
      (getChoiceAmount "Resources" -> n)
      ( ProxyTarget
          (isTarget attrs -> True)
          (ProxyTarget (InvestigatorTarget fromInvestigator) (InvestigatorTarget toInvestigator))
        ) ->
        do
          pushAll
            [ TakeResources toInvestigator n (toSource attrs) False
            , SpendResources fromInvestigator n
            ]
          pure l
    _ -> WitchHauntedWoodsAbandonedMine <$> liftRunMessage msg attrs
